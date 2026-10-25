#####################################################
### Live Solver Engine                            ###
###                                               ###
### Purpose: Reusable core for the Live Solver    ###
###  (Shiny app) and the headless runner. Builds  ###
###  installation / decommissioning scenarios     ###
###  under user constraints, writes the habitat   ###
###  maps, drives the Ecospace console and scores ###
###  each run.                                    ###
#####################################################
#
# Workflow per iteration (same as structure_optimization.R):
#   1. Generate a candidate strategy (cells to install and/or decommission)
#   2. Apply it to the pristine structure + natural habitat layers
#   3. Archive the maps in the Run folder
#   4. Make the maps visible to Ecospace (overwrite Inputs, or per-run STConfig)
#   5. Write a per-run command file and launch EwEClientConsole.exe
#   6. Read the objectives from the output CSVs and score the run
#
# All functions are prefixed with `ls_` (Live Solver).

# Load libraries and dependencies ----
library(terra)
library(xml2)
library(jsonlite)

# Constants ----
ls_default_console <- "C:/Program Files/EcoSpace Console 1.4.3.1 64-bit/EwEClientConsole.exe"
ls_lock_name       <- ".livesolver_lock"

# Build stamp written by build_package.R ("development" when running from the repository)
ls_version <- function() {
  f <- file.path(Sys.getenv("OPTIMAL_ECOSPACE_ROOT", "../.."), "R/LiveSolver/VERSION")
  if (file.exists(f)) readLines(f, warn = FALSE)[1] else "development"
}

# Small helpers ----

# NA / NULL / "" -> no limit
ls_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- suppressWarnings(as.numeric(x[1]))
  if (is.na(x)) default else x
}
ls_limit <- function(x) ls_num(x, Inf)

ls_winpath <- function(p) {
  if (.Platform$OS.type == "windows") normalizePath(p, winslash = "\\", mustWork = FALSE) else p
}

ls_timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

ls_pct <- function(x) paste0(trimws(formatC(100 * x, format = "fg", digits = 3)), "%")

ls_usd <- function(x) paste0("$", formatC(round(x), format = "f", digits = 0, big.mark = ","))

# Uniform integer between a and b (inclusive); NA when infeasible
ls_sample_between <- function(a, b) {
  a <- ceiling(a); b <- floor(b)
  if (!is.finite(b) || b < a) return(NA_integer_)
  as.integer(a + sample.int(b - a + 1, 1) - 1)
}

# Weighted random ordering of a vector (sample() is unsafe when length == 1)
ls_shuffle <- function(x, prob = NULL) {
  if (length(x) <= 1) return(x)
  if (!is.null(prob)) {
    prob[!is.finite(prob) | prob < 0] <- 0
    if (sum(prob) <= 0) prob <- NULL
  }
  x[sample.int(length(x), prob = prob)]
}

# Cells (1-based, row-major as in terra) within a Chebyshev radius of a cell
ls_neighbors <- function(idx, radius, nr, nc) {
  r  <- (idx - 1) %/% nc
  c  <- (idx - 1) %% nc
  rr <- max(0, r - radius):min(nr - 1, r + radius)
  cc <- max(0, c - radius):min(nc - 1, c + radius)
  as.vector(outer(cc, rr * nc, "+")) + 1
}

# Boolean vector: cells within `radius` of any of `idx`
ls_within <- function(idx, radius, nr, nc) {
  hit <- logical(nr * nc)
  for (i in idx) hit[ls_neighbors(i, radius, nr, nc)] <- TRUE
  hit
}

# Model Discovery ----

# Every Model/<region>/*.eweaccdb (backups end in .backup_* and are skipped)
ls_discover_models <- function(repo_root) {
  model_root <- file.path(repo_root, "Model")
  regions    <- list.dirs(model_root, full.names = FALSE, recursive = FALSE)
  rows <- lapply(regions, function(reg) {
    files <- list.files(file.path(model_root, reg), pattern = "\\.eweaccdb$", full.names = TRUE)
    if (!length(files)) return(NULL)
    data.frame(
      region     = reg,
      model_name = sub("\\.eweaccdb$", "", basename(files)),
      model_file = normalizePath(files, winslash = "/"),
      inputs_dir = normalizePath(file.path(repo_root, "Inputs", reg), winslash = "/", mustWork = FALSE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(region = character(), model_name = character(),
                      model_file = character(), inputs_dir = character())
  }
  out
}

# Read the spatial-temporal config and locate every habitat layer locally
ls_read_stconfig <- function(stconfig, inputs_dir) {
  doc <- read_xml(stconfig)
  xml_ns_strip(doc)
  confs <- xml_find_all(doc, "//Dataset/Configuration")
  layers <- data.frame(
    name     = xml_text(xml_find_first(confs, "Name")),
    variable = xml_text(xml_find_first(confs, "Variable")),
    file_xml = xml_text(xml_find_first(confs, "File")),
    guid     = xml_attr(xml_parent(confs), "GUID"),
    stringsAsFactors = FALSE
  )
  # STConfig stores absolute Windows paths; resolve by file name inside Inputs/<region>
  local_name <- basename(gsub("\\\\", "/", layers$file_xml))
  local_name[is.na(local_name) | local_name == ""] <- paste0(layers$name, ".asc")
  layers$file <- normalizePath(file.path(inputs_dir, local_name), winslash = "/", mustWork = FALSE)
  layers$exists <- file.exists(layers$file)
  layers
}

ls_region_info <- function(inputs_dir) {
  top <- list.files(inputs_dir, full.names = TRUE)
  stconfig <- top[grepl("STConfig\\.xml$", top, ignore.case = TRUE)]
  cmd      <- top[grepl("CommandFile.*\\.txt$", top, ignore.case = TRUE)]
  if (!length(cmd)) {
    txt <- top[grepl("\\.txt$", top, ignore.case = TRUE)]
    cmd <- txt[vapply(txt, function(f) any(grepl("<EWE_MODEL_FILE>", readLines(f, warn = FALSE), fixed = TRUE)), logical(1))]
  }
  asc <- basename(top[grepl("\\.asc$", top, ignore.case = TRUE)])
  layers <- if (length(stconfig)) ls_read_stconfig(stconfig[1], inputs_dir) else NULL
  list(
    inputs_dir   = inputs_dir,
    stconfig     = if (length(stconfig)) stconfig[1] else NA_character_,
    cmd_template = if (length(cmd)) cmd[1] else NA_character_,
    asc_files    = asc,
    layers       = layers,
    has_backup   = dir.exists(file.path(inputs_dir, "Backup_Base_Maps")),
    locked       = file.exists(file.path(inputs_dir, ls_lock_name))
  )
}

# Looks in a `console/` folder next to the app first (portable package), then Program Files
ls_find_console <- function(repo_root = NULL) {
  local <- if (!is.null(repo_root) && dir.exists(file.path(repo_root, "console"))) {
    list.files(file.path(repo_root, "console"), pattern = "^EwEClientConsole\\.exe$",
               recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  } else character()
  hits <- c(local, Sys.glob(c("C:/Program Files/EcoSpace Console*/EwEClientConsole.exe",
                              "C:/Program Files (x86)/EcoSpace Console*/EwEClientConsole.exe")))
  if (length(hits)) normalizePath(hits[1], winslash = "/") else ls_default_console
}

# Default Configuration ----

# Objectives follow structure_optimization.R. Targets may list several groups separated by commas
# (columns matching any of them are summed); fleet restricts catch/revenue columns to one fleet.
# Weight-0 objectives are reported but do not change the composite score or the Pareto front.
ls_fed_species <- c("greater amberjack", "gag", "red grouper", "red snapper",
                    "gray snapper", "lane snapper", "gray triggerfish", "vermilion snapper")

ls_default_objectives <- function() {
  B <- "Annual_Average_Biomass"; C <- "Annual_Average_Catch"
  fed <- paste(ls_fed_species, collapse = ", ")
  main <- data.frame(
    name         = c("RS_Biomass", "Tot_Biomass", "Fed_Biomass", "RS_Catch", "Tot_Catch", "Fed_Catch",
                     "Comm_Catch", "Rec_Catch", "Total_Revenue", "Comm_Revenue", "Biodiversity", "Lionfish_Biomass"),
    file_pattern = c(B, B, B, C, C, C, C, C, C, C, B, B),
    target       = c("red snapper adult", NA, fed, "red snapper adult", NA, fed, fed, fed, NA, NA, NA, "lionfish"),
    type         = c("species", "sum", "species", "species", "sum", "species", "species", "species",
                     "revenue", "revenue", "biodiversity", "species"),
    direction    = c(rep("max", 11), "min"),
    weight       = c(1, 0.5, 1, 2, 0.5, 1, 1, 1, 2, 0, 1, 1),
    fleet        = c(NA, NA, NA, NA, NA, NA, "commercial", "recreational", NA, "commercial", NA, NA),
    stringsAsFactors = FALSE
  )
  slug <- gsub("[^A-Za-z0-9]+", "_", tolower(trimws(ls_fed_species)))
  per_species <- data.frame(
    name         = c(rbind(paste0("FedSp_", slug, "_Biomass"), paste0("FedSp_", slug, "_Catch"))),
    file_pattern = rep(c(B, C), length(slug)),
    target       = rep(ls_fed_species, each = 2),
    type = "species", direction = "max", weight = 0, fleet = NA, stringsAsFactors = FALSE
  )
  rbind(main, per_species)
}

# Economics used by "revenue" objectives and the reporting columns (structure_optimization.R econ_params)
ls_default_econ <- function() {
  list(
    lbs_per_mt    = 2204.62,
    default_price = 1.00,   # $/lb for groups without a listed price
    fleets = data.frame(fleet = c("commercial", "recreational", "lionfish"), multiplier = c(1, 1, 1),
                        stringsAsFactors = FALSE),
    prices = data.frame(
      species = c("greater amberjack", "gag", "red grouper", "gray snapper", "lane snapper",
                  "red snapper", "vermilion snapper", "gray triggerfish"),
      price   = c(1.38, 4.69, 3.64, 2.77, 2.62, 4.21, 2.81, 2.29),
      stringsAsFactors = FALSE)
  )
}

# Machine-specific settings (console and cache locations) remembered between sessions
ls_settings_file <- function(repo_root) file.path(repo_root, "LiveSolver_settings.json")

ls_read_settings <- function(repo_root) {
  f <- ls_settings_file(repo_root)
  if (!file.exists(f)) return(list())
  tryCatch(fromJSON(f), error = function(e) list())
}

ls_save_settings <- function(repo_root, cfg) {
  tryCatch(write_json(list(console_path = cfg$model$console_path, cache_dir = cfg$model$cache_dir %||% ""),
                      ls_settings_file(repo_root), auto_unbox = TRUE, pretty = TRUE),
           error = function(e) NULL)
}

# Values in the command-file template, by tag (e.g. "<N_ECOSPACE_YEARS>" -> "30")
ls_read_cmd_tags <- function(template) {
  if (is.null(template) || is.na(template) || !file.exists(template)) return(character())
  lines <- readLines(template, warn = FALSE)
  hit <- grep("^\\s*<[A-Z_]+>\\s*,", lines, value = TRUE)
  setNames(trimws(sub("^\\s*<[A-Z_]+>\\s*,\\s*([^,]*).*$", "\\1", hit)), sub("^\\s*(<[A-Z_]+>).*$", "\\1", hit))
}

# Run settings shown in the app, read from the region's command-file template
ls_template_run_settings <- function(template) {
  t <- ls_read_cmd_tags(template)
  n <- function(tag, d) { v <- suppressWarnings(as.numeric(t[tag])); if (length(v) && !is.na(v)) v else d }
  list(
    ecosim_scenario   = n("<ECOSIM_SCENARIO_INDEX>", 1),
    ecospace_scenario = n("<ECOSPACE_SCENARIO_INDEX>", 1),
    n_years           = n("<N_ECOSPACE_YEARS>", 30),
    use_spinup        = !identical(tolower(unname(t["<ECOSPACE_USE_SPINUP>"])), "false"),
    spinup_years      = n("<ECOSPACE_SPINUP_LENGTH>", 5),
    timesteps_year    = n("<N_ECOSPACE_TIMESTEPS_YEAR>", 12),
    n_threads         = n("<N_BIOMASS_THREADS>", 15)
  )
}

ls_default_config <- function(repo_root) {
  models <- ls_discover_models(repo_root)
  m      <- if (nrow(models)) models[1, ] else NULL
  info   <- if (!is.null(m)) ls_region_info(m$inputs_dir) else NULL
  lyr    <- info$layers
  struct_layer <- if (!is.null(lyr)) {
    hit <- grep("struct|reef", lyr$name, ignore.case = TRUE, value = TRUE)
    if (length(hit)) hit[1] else lyr$name[1]
  } else NA_character_
  saved   <- ls_read_settings(repo_root)
  console <- if (!is.null(saved$console_path) && file.exists(saved$console_path)) saved$console_path else ls_find_console(repo_root)
  run     <- ls_template_run_settings(info$cmd_template)

  list(
    model = c(list(
      region            = if (!is.null(m)) m$region else NA,
      model_file        = if (!is.null(m)) m$model_file else NA,
      inputs_dir        = if (!is.null(m)) m$inputs_dir else NA,
      stconfig          = if (!is.null(info)) info$stconfig else NA,
      cmd_template      = if (!is.null(info)) info$cmd_template else NA,
      console_path      = console),
      run,              # scenario indices, years, spin-up, time steps, threads (template values)
      list(
      timeout_sec       = 1800,
      cache_dir         = saved$cache_dir %||% "",  # Ecospace's spatial data cache folder, emptied before every run
      extra_tags        = ""      # raw command-file lines appended verbatim (e.g. vulnerabilities)
    )),
    layers = list(
      structure_layer   = struct_layer,
      existing_file     = "",    # "" = use the pristine structure layer as existing structures
      natural_habitats  = if (!is.null(lyr)) setdiff(lyr$name, struct_layer) else character(),
      basemap_file      = "basemap.asc",
      pristine_source   = "auto",  # auto: Inputs/<region>/Initial Base Mapps when present, else Inputs;
                                   # inputs: current Inputs files; backup: Backup_Base_Maps
      existing_cover    = "auto"   # auto: keep fractional maps, give presence (0/1) maps one module's cover;
                                   # as_mapped: keep values; one_module: one module's cover per structure cell
    ),
    strategy = list(
      type          = "install",  # install | decommission | mixed
      shared_budget = FALSE        # mixed only: decommissioning spends from the installation budget
    ),
    # Reef modules (structure_optimization.R reef_params): standard prefab concrete modules
    install = list(
      budget                = 6000000, # each scenario draws a random budget up to this (USD)
      unit_cost             = 2500,    # deployed cost per module (USD)
      depth_cost_factor     = 0,       # > 0 makes deeper deployment cost more per module
      ref_depth_m           = 30,      # reference depth for the depth cost factor
      unit_footprint_m2     = 6,       # module base area
      unit_height_m         = 3,       # module vertical relief
      min_clearance_m       = 6,       # water required above the crest
      halo_base_m           = 500,     # radius of a module's ecological halo, grows with reef neighbours
      max_reef_fraction     = 1,       # cap on the share of a cell that is reef
      max_modules_per_drop  = 100,     # most modules stacked in a cell in one drop
      max_batch_discount    = 0.30,    # cost saving per module for a full drop
      enforce_connectivity  = FALSE,   # favour cells next to existing modules
      connectivity_bonus    = 10,
      depth_min             = NA,
      depth_max             = NA,
      use_exclusions        = TRUE,
      exclusions_file       = "exclusions.asc",
      extra_mask_file       = "",      # optional .asc; cells > 0 are allowed
      xmin = NA, xmax = NA, ymin = NA, ymax = NA,
      buffer_existing       = 0,       # cells kept clear around existing structures
      avoid_habitats        = character(),
      avoid_threshold       = 0.5,     # skip cells where avoided habitats sum above this
      displacement          = "proportional" # proportional (natural habitat makes room for reef) | none
    ),
    decom = list(
      budget            = 5000000,
      min_n             = 1,
      max_n             = 50,
      min_retained      = 0,
      eligible_threshold = 0,          # existing structure value must exceed this
      removal_fraction  = 1.0,         # 1 = full removal, < 1 = partial (e.g. toppling)
      cost_source       = "fixed",     # fixed | depth | map
      cost_map_file     = "cost_map.asc",
      cost_multiplier   = 1,
      fixed_cost        = 50000,
      depth_cost_rate   = 1000,
      depth_min         = NA,
      depth_max         = NA,
      xmin = NA, xmax = NA, ymin = NA, ymax = NA,
      selection         = "random",   # random | cheapest | costliest | shallowest | deepest
      restore           = "proportional" # proportional | none | <natural habitat name>
    ),
    search = list(
      iterations        = 100,
      algorithm         = "random",   # random | evolutionary (mutates runs on the Pareto front)
      warmup            = 200,        # random runs before evolutionary mutation starts
      mutation_min      = 0.1,        # share of a parent's reef cells removed when mutating
      mutation_max      = 0.4,
      seed              = NA,
      dry_run           = !file.exists(console),
      eval_last_n       = 5,          # objectives are means over the last N years
      roi_objective     = "RS_Catch",
      revenue_objective = "Total_Revenue",
      max_retries       = 50
    ),
    econ = ls_default_econ(),
    objectives = ls_default_objectives()
  )
}

# Config persistence (NA = "no limit" survives the JSON round-trip as null)
ls_save_config <- function(cfg, path) {
  write_json(cfg, path, auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "null")
}

ls_load_config <- function(path, repo_root) {
  raw  <- fromJSON(path, simplifyVector = TRUE)
  base <- ls_default_config(repo_root)
  for (sec in setdiff(names(base), c("objectives", "econ"))) {
    if (!is.null(raw[[sec]])) {
      for (k in names(raw[[sec]])) {
        v <- raw[[sec]][[k]]
        base[[sec]][[k]] <- if (is.null(v)) NA else v
      }
    }
  }
  if (!is.null(raw$objectives)) {
    o <- as.data.frame(raw$objectives, stringsAsFactors = FALSE)
    if (is.null(o$fleet)) o$fleet <- NA_character_
    base$objectives <- o
  }
  if (!is.null(raw$econ)) {
    for (k in c("lbs_per_mt", "default_price")) if (!is.null(raw$econ[[k]])) base$econ[[k]] <- raw$econ[[k]]
    for (k in c("fleets", "prices")) if (!is.null(raw$econ[[k]])) base$econ[[k]] <- as.data.frame(raw$econ[[k]], stringsAsFactors = FALSE)
  }
  base$layers$natural_habitats <- as.character(unlist(base$layers$natural_habitats))
  base$install$avoid_habitats  <- as.character(unlist(base$install$avoid_habitats))
  base
}

# Problem Construction ----

ls_read_layer <- function(path, template, method = "near") {
  r <- rast(path)
  if (!compareGeom(template, r, stopOnError = FALSE)) r <- resample(r, template, method = method)
  values(r, mat = FALSE)
}

ls_resolve <- function(file, inputs_dir) {
  if (is.null(file) || is.na(file) || file == "") return(NA_character_)
  if (file.exists(file)) return(normalizePath(file, winslash = "/"))
  p <- file.path(inputs_dir, file)
  if (file.exists(p)) normalizePath(p, winslash = "/") else NA_character_
}

ls_cost_vector <- function(spec, depth, inputs_dir, template) {
  n <- length(depth)
  cost <- switch(spec$cost_source,
    map = {
      f <- ls_resolve(spec$cost_map_file, inputs_dir)
      if (is.na(f)) stop("Cost map not found: ", spec$cost_map_file)
      ls_read_layer(f, template, "bilinear") * ls_num(spec$cost_multiplier, 1)
    },
    depth = depth * ls_num(spec$depth_cost_rate, 0),
    fixed = rep(0, n),
    stop("Unknown cost source: ", spec$cost_source)
  )
  cost <- cost + ls_num(spec$fixed_cost, 0)
  cost[is.na(cost) | cost < 0] <- 0
  cost
}

ls_bbox_mask <- function(spec, xy) {
  ok <- rep(TRUE, nrow(xy))
  if (!is.na(ls_num(spec$xmin))) ok <- ok & xy[, 1] >= ls_num(spec$xmin)
  if (!is.na(ls_num(spec$xmax))) ok <- ok & xy[, 1] <= ls_num(spec$xmax)
  if (!is.na(ls_num(spec$ymin))) ok <- ok & xy[, 2] >= ls_num(spec$ymin)
  if (!is.na(ls_num(spec$ymax))) ok <- ok & xy[, 2] <= ls_num(spec$ymax)
  ok
}

ls_depth_mask <- function(spec, depth) {
  ok <- rep(TRUE, length(depth))
  if (!is.na(ls_num(spec$depth_min))) ok <- ok & depth >= ls_num(spec$depth_min)
  if (!is.na(ls_num(spec$depth_max))) ok <- ok & depth <= ls_num(spec$depth_max)
  ok & !is.na(depth)
}

# If a previous session crashed while Inputs maps were overwritten, put the originals back
ls_recover_inputs <- function(inputs_dir) {
  lock <- file.path(inputs_dir, ls_lock_name)
  if (!file.exists(lock)) return(FALSE)
  info <- tryCatch(fromJSON(lock), error = function(e) NULL)
  if (!is.null(info) && dir.exists(info$snapshot_dir)) {
    for (i in seq_along(info$files)) {
      src <- if (!is.null(info$sources)) info$sources[i] else file.path(info$snapshot_dir, basename(info$files[i]))
      if (file.exists(src)) file.copy(src, info$files[i], overwrite = TRUE)
    }
    message("Recovered pristine Inputs maps from interrupted session: ", info$snapshot_dir)
  }
  file.remove(lock)
  TRUE
}

# The script's pristine map folder (Inputs/<region>/Initial Base Mapps) or Backup_Base_Maps
ls_pristine_dir <- function(inputs_dir, source = "auto") {
  if (identical(source, "inputs")) return(NULL)
  if (identical(source, "backup")) {
    d <- file.path(inputs_dir, "Backup_Base_Maps")
    return(if (dir.exists(d)) d else NULL)
  }
  d <- list.dirs(inputs_dir, recursive = FALSE)
  d <- d[grepl("^initial base map", basename(d), ignore.case = TRUE)]
  if (length(d)) d[1] else NULL
}

# 8-neighbour (Moore) cells of every cell
ls_neighbor_list <- function(nr, nc) {
  lapply(seq_len(nr * nc), function(i) {
    r <- (i - 1) %/% nc; c <- (i - 1) %% nc
    rr <- r + c(-1, -1, -1, 0, 0, 1, 1, 1); cc <- c + c(-1, 0, 1, -1, 1, -1, 0, 1)
    ok <- rr >= 0 & rr < nr & cc >= 0 & cc < nc
    rr[ok] * nc + cc[ok] + 1
  })
}

# Reef module model (structure_optimization.R) ----

ls_usable_relief <- function(depth_m, I) {
  clr      <- ls_num(I$min_clearance_m, 6)
  feasible <- !is.na(depth_m) & depth_m > clr
  usable   <- pmin(ls_num(I$unit_height_m, 3), pmax(0, depth_m - clr))
  usable[!feasible] <- 0
  list(usable_height_m = usable, relative_relief = ifelse(!is.na(depth_m) & depth_m > 0, usable / depth_m, 0),
       feasible = feasible)
}

ls_unit_cost <- function(depth_m, I) {
  ls_num(I$unit_cost, 2500) * (1 + ls_num(I$depth_cost_factor, 0) * (depth_m / ls_num(I$ref_depth_m, 30)))
}

# Per-cell metrics for n modules: the halo grows with the number of reef neighbours, and
# unit_fraction is the share of the cell one module's footprint + halo covers
ls_reef_metrics <- function(depth_m, n_units, cell_area_m2, cell_idx, reef_mask, neighbors, I) {
  k <- length(depth_m)
  n_units <- rep_len(n_units, k); cell_area_m2 <- rep_len(cell_area_m2, k); cell_idx <- rep_len(cell_idx, k)
  rel <- ls_usable_relief(depth_m, I)
  n_units[!rel$feasible] <- 0
  r0 <- sqrt(ls_num(I$unit_footprint_m2, 6) / pi)
  n_contig <- vapply(cell_idx, function(i) sum(reef_mask[neighbors[[i]]] > 0, na.rm = TRUE), numeric(1))
  halo_R   <- ifelse(rel$feasible, ls_num(I$halo_base_m, 500) * (1 + n_contig), 0)
  unit_fraction <- pi * (r0 + halo_R)^2 / cell_area_m2
  unit_fraction[!rel$feasible] <- 0
  data.frame(n_units = n_units, usable_height_m = rel$usable_height_m, relative_relief = rel$relative_relief,
             halo_radius_m = halo_R, n_contig = n_contig,
             footprint_frac = n_units * ls_num(I$unit_footprint_m2, 6) / cell_area_m2,
             unit_fraction = unit_fraction)
}

# Cost of deploying `module_counts` per cell as barge drops of up to max_drop modules; fuller drops
# get up to max_disc off per module
ls_deployment_cost <- function(module_counts, unit_costs, max_drop, max_disc) {
  full <- floor(module_counts / max_drop)
  rem  <- module_counts %% max_drop
  disc_rem <- max_disc * ((rem - 1) / pmax(1, max_drop - 1))
  disc_rem[rem <= 0] <- 0
  full * (max_drop * unit_costs * (1 - max_disc)) + rem * unit_costs * (1 - disc_rem)
}

# Coverage after adding modules: approaches the cap asymptotically (Poisson overlap)
ls_poisson_cover <- function(current, increment, cap) cap - (cap - pmin(cap, current)) * exp(-increment / cap)

# Everything that stays fixed for a whole session
# recover = FALSE for previews: never touch the Inputs folder while a session may be running
ls_build_problem <- function(cfg, session_dir = NULL, recover = TRUE) {
  M <- cfg$model; L <- cfg$layers; I <- cfg$install
  inputs_dir <- M$inputs_dir
  if (is.na(inputs_dir) || !dir.exists(inputs_dir)) stop("Inputs directory not found: ", inputs_dir)
  recovered <- recover && ls_recover_inputs(inputs_dir)

  layers <- ls_read_stconfig(M$stconfig, inputs_dir)
  if (!L$structure_layer %in% layers$name) stop("Structure layer '", L$structure_layer, "' is not in the STConfig file.")
  natural <- setdiff(intersect(L$natural_habitats, layers$name), L$structure_layer)
  used    <- layers[layers$name %in% c(L$structure_layer, natural), ]
  missing <- used$name[!used$exists]
  if (length(missing)) stop("Layer file(s) missing in Inputs: ", paste(missing, collapse = ", "))

  # Pristine maps: the script's "Initial Base Mapps" folder when present; the active Inputs maps are
  # snapshotted separately so they can be put back exactly as they were
  pristine_dir  <- ls_pristine_dir(inputs_dir, L$pristine_source %||% "auto")
  pristine_file <- function(f) {
    if (!is.null(pristine_dir)) { p <- file.path(pristine_dir, basename(f)); if (file.exists(p)) return(p) }
    f
  }
  snapshot_dir <- NULL
  if (!is.null(session_dir)) {
    snapshot_dir <- file.path(session_dir, "_pristine")
    dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)
    for (f in used$file) file.copy(f, file.path(snapshot_dir, basename(f)), overwrite = TRUE)
  }

  basemap_file <- ls_resolve(L$basemap_file, inputs_dir)
  if (is.na(basemap_file)) stop("Basemap not found: ", L$basemap_file)
  template <- rast(pristine_file(basemap_file))
  nr <- nrow(template); nc <- ncol(template)
  depth <- values(template, mat = FALSE)
  water <- !is.na(depth) & depth > 0
  xy    <- xyFromCell(template, seq_len(ncell(template)))
  cell_area_m2 <- values(cellSize(template, unit = "m"), mat = FALSE)
  neighbors <- ls_neighbor_list(nr, nc)

  struct_file <- used$file[used$name == L$structure_layer]
  struct0 <- ls_read_layer(pristine_file(struct_file), template)
  struct0[is.na(struct0)] <- 0
  raw_hab <- setNames(lapply(natural, function(h) ls_read_layer(pristine_file(used$file[used$name == h]), template, "bilinear")), natural)

  existing_f <- ls_resolve(L$existing_file, inputs_dir)
  existing <- if (!is.na(existing_f)) ls_read_layer(existing_f, template) else struct0
  existing[is.na(existing)] <- 0

  # Existing reef cover: presence maps (all 0/1) get one module's coverage per structure cell
  cover_mode <- L$existing_cover %||% "auto"
  fractional <- any(struct0 > 0 & struct0 < 1)
  if (cover_mode == "one_module" || (cover_mode == "auto" && !fractional)) {
    has <- which(struct0 > 0)
    if (length(has)) {
      # Base halo only: a presence map says nothing about how many structures share a cell
      uf <- ls_reef_metrics(depth[has], 1, cell_area_m2[has], has, rep(FALSE, length(struct0)), neighbors, I)$unit_fraction
      if (any(uf > 0)) uf[uf <= 0] <- stats::median(uf[uf > 0])
      struct0[has] <- pmin(ls_num(I$max_reef_fraction, 1), uf)
    }
  }

  # Natural habitats are rescaled to the room the reef leaves in each cell (natural + reef <= 1)
  nat_sum <- if (length(raw_hab)) Reduce(`+`, raw_hab) else rep(NA_real_, length(depth))
  room    <- pmin(1, pmax(0, 1 - struct0))
  has_nat <- !is.na(nat_sum) & nat_sum > 0
  scale   <- rep(0, length(depth)); scale[has_nat] <- room[has_nat] / nat_sum[has_nat]
  hab0    <- lapply(raw_hab, function(v) v * scale)
  edge    <- !has_nat & struct0 > 0 & "sand" %in% names(hab0)
  if (any(edge)) hab0[["sand"]][edge] <- room[edge]

  # Installation feasibility ----
  rel <- ls_usable_relief(depth, I)
  inst_ok <- !is.na(depth) & rel$feasible
  if (isTRUE(I$use_exclusions)) {
    ef <- ls_resolve(I$exclusions_file, inputs_dir)
    if (is.na(ef)) stop("Exclusions map not found: ", I$exclusions_file)
    ex <- ls_read_layer(pristine_file(ef), template)
    ex[is.na(ex)] <- 0
    inst_ok <- inst_ok & ex > 0.5
  }
  mf <- ls_resolve(I$extra_mask_file, inputs_dir)
  if (!is.na(mf)) {
    mk <- ls_read_layer(mf, template)
    inst_ok <- inst_ok & !is.na(mk) & mk > 0
  }
  inst_ok <- inst_ok & ls_depth_mask(I, depth) & ls_bbox_mask(I, xy)
  if (ls_num(I$buffer_existing, 0) > 0 && any(existing > 0)) {
    inst_ok <- inst_ok & !ls_within(which(existing > 0), ls_num(I$buffer_existing), nr, nc)
  }
  avoid <- intersect(I$avoid_habitats, natural)
  if (length(avoid)) {
    hard <- Reduce(`+`, lapply(raw_hab[avoid], function(v) { v[is.na(v)] <- 0; v }))
    inst_ok <- inst_ok & hard <= ls_num(I$avoid_threshold, 1)
  }
  unit_cost <- ls_unit_cost(depth, I)
  unit_cost[!rel$feasible | is.na(unit_cost)] <- Inf
  valid <- which(inst_ok)
  cap_increment <- numeric(length(depth))
  if (length(valid)) {
    cap_increment[valid] <- ls_reef_metrics(depth[valid], 1, cell_area_m2[valid], valid, struct0 > 0, neighbors, I)$unit_fraction
  }

  # Decommissioning feasibility ----
  D <- cfg$decom
  dec_ok <- existing > ls_num(D$eligible_threshold, 0) & struct0 > 0 &
    ls_depth_mask(D, depth) & ls_bbox_mask(D, xy)
  dec_cost <- ls_cost_vector(D, depth, inputs_dir, template)

  type <- cfg$strategy$type
  P <- list(
    cfg = cfg, session_dir = session_dir, snapshot_dir = snapshot_dir, recovered = recovered,
    pristine_dir = pristine_dir,
    template = template, nr = nr, nc = nc, depth = depth, water = water, xy = xy,
    cell_area_m2 = cell_area_m2, neighbors = neighbors,
    total_area_km2 = sum(cell_area_m2[!is.na(depth)], na.rm = TRUE) / 1e6,
    layers = used, struct_file = struct_file, natural = natural,
    struct0 = struct0, hab0 = hab0, existing = existing,
    install_valid  = if (type %in% c("install", "mixed")) valid else integer(0),
    decom_eligible = if (type %in% c("decommission", "mixed")) which(dec_ok) else integer(0),
    unit_cost = unit_cost, cap_increment = cap_increment, decom_cost = dec_cost
  )
  P$summary <- ls_problem_summary(P)
  P
}

ls_problem_summary <- function(P) {
  I  <- P$cfg$install
  uc <- P$unit_cost[P$install_valid]
  uf <- P$cap_increment[P$install_valid]
  dc <- P$decom_cost[P$decom_eligible]
  rng <- function(x, f) if (length(x)) { r <- range(x); if (diff(r) < 1e-9 * max(1, abs(r[1]))) f(r[1]) else paste(f(r[1]), "-", f(r[2])) } else "-"
  data.frame(
    item = c("Grid (rows x cols)", "Water cells", "Pristine maps", "Existing structure cells",
             "Existing structure cover (min - max of cell)", "Cells eligible for modules",
             "Cost per module", "Cell cover from one module", "Budget per scenario",
             "Structures eligible for decommissioning", "Removal cost"),
    value = c(
      sprintf("%d x %d", P$nr, P$nc), sum(P$water),
      if (is.null(P$pristine_dir)) "Inputs folder" else basename(P$pristine_dir),
      sum(P$struct0 > 0),
      if (any(P$struct0 > 0)) paste(ls_pct(min(P$struct0[P$struct0 > 0])), "-", ls_pct(max(P$struct0))) else "-",
      length(P$install_valid),
      rng(uc, ls_usd), rng(pmin(1, uf), ls_pct),
      paste("random, up to", ls_usd(ls_num(I$budget, 0))),
      length(P$decom_eligible), rng(dc, ls_usd)
    ),
    stringsAsFactors = FALSE
  )
}

# Candidate Generation ----

ls_affordable <- function(cost, budget) sum(cumsum(sort(cost)) <= budget)

# Modules allocated as random batch drops until a randomly drawn budget is spent
# (structure_optimization.R batch-stacking loop). `initial` = modules kept from a parent run.
ls_gen_modules <- function(P, budget_limit, initial = NULL) {
  I <- P$cfg$install
  n <- length(P$depth)
  valid <- P$install_valid
  none  <- list(cells = integer(0), n = numeric(0), cost = 0, budget = 0)
  if (!length(valid) || !is.finite(budget_limit) || budget_limit <= 0) return(none)
  cap       <- ls_num(I$max_reef_fraction, 1)
  max_drop  <- max(1, round(ls_num(I$max_modules_per_drop, 100)))
  max_disc  <- ls_num(I$max_batch_discount, 0.3)
  bonus     <- ls_num(I$connectivity_bonus, 10)
  connect   <- isTRUE(I$enforce_connectivity)
  uc        <- P$unit_cost

  added <- numeric(n)
  capacity <- P$struct0
  spent <- 0
  if (!is.null(initial) && length(initial$cells)) {
    added[initial$cells] <- initial$n
    spent <- sum(ls_deployment_cost(added[valid], uc[valid], max_drop, max_disc), na.rm = TRUE)
    for (c in initial$cells) capacity[c] <- ls_poisson_cover(capacity[c], P$cap_increment[c] * added[c], cap)
  }

  min_site <- min(uc[valid])
  lo <- max(spent, min_site)
  hi <- max(max(budget_limit, min_site), lo)
  run_budget <- stats::runif(1, lo, hi)

  weights <- rep(1, n)
  if (connect && !is.null(initial)) {
    for (c in initial$cells) weights[P$neighbors[[c]]] <- weights[P$neighbors[[c]]] + bonus * added[c]
  }

  for (guard in seq_len(1e5)) {
    if (spent >= run_budget) break
    eligible <- valid[uc[valid] <= (run_budget - spent) & capacity[valid] < (cap - 1e-6)]
    if (!length(eligible)) break
    cell <- if (length(eligible) == 1) eligible else sample(eligible, 1, prob = weights[eligible])
    max_by_budget <- floor((run_budget - spent) / uc[cell])
    if (max_by_budget < 1) break
    batch <- sample.int(min(max_drop, max_by_budget), 1)
    old_cost <- ls_deployment_cost(added[cell], uc[cell], max_drop, max_disc)
    added[cell] <- added[cell] + batch
    spent <- spent + ls_deployment_cost(added[cell], uc[cell], max_drop, max_disc) - old_cost
    capacity[cell] <- ls_poisson_cover(capacity[cell], P$cap_increment[cell] * batch, cap)
    if (connect) weights[P$neighbors[[cell]]] <- weights[P$neighbors[[cell]]] + bonus * batch
  }
  cells <- which(added > 0)
  list(cells = cells, n = added[cells], cost = spent, budget = run_budget)
}

ls_decom_order <- function(P, pool) {
  D <- P$cfg$decom
  c <- P$decom_cost[pool]; d <- P$depth[pool]
  switch(D$selection,
    cheapest   = ls_shuffle(pool, 1 / pmax(c, 1)),
    costliest  = ls_shuffle(pool, c),
    shallowest = ls_shuffle(pool, 1 / pmax(d, 0.1)),
    deepest    = ls_shuffle(pool, d),
    ls_shuffle(pool)
  )
}

ls_gen_decom <- function(P, budget, keep = integer(0)) {
  D <- P$cfg$decom
  pool <- P$decom_eligible
  if (!length(pool)) return(list(cells = integer(0), cost = 0))
  n_exist <- sum(P$existing > 0)
  max_n <- min(length(pool), ls_limit(D$max_n), n_exist - ls_num(D$min_retained, 0))
  target <- ls_sample_between(ls_num(D$min_n, 0), min(max_n, ls_affordable(P$decom_cost[pool], budget)))
  if (is.na(target)) return(list(cells = integer(0), cost = 0, infeasible = TRUE))
  keep  <- intersect(keep, pool)
  order <- c(ls_shuffle(keep), setdiff(ls_decom_order(P, pool), keep))
  chosen <- integer(0); spent <- 0
  for (i in order) {
    if (length(chosen) >= target) break
    if (spent + P$decom_cost[i] > budget) next
    chosen <- c(chosen, i); spent <- spent + P$decom_cost[i]
  }
  list(cells = chosen, cost = spent)
}

ls_candidate_key <- function(cand) {
  paste(paste(cand$install, cand$modules, sep = ":", collapse = "."), paste(sort(cand$decom), collapse = "."), sep = "|")
}

ls_check_candidate <- function(P, cand) {
  type <- P$cfg$strategy$type
  notes <- character()
  if (type %in% c("decommission", "mixed") && length(cand$decom) < ls_num(P$cfg$decom$min_n, 0))
    notes <- c(notes, "fewer removals than the minimum")
  cand$feasible <- !length(notes)
  cand$note <- paste(notes, collapse = "; ")
  cand
}

# kind: baseline_none | random | evolutionary (mutates `parent`, as the script's MOEA mode)
ls_generate_candidate <- function(P, kind = "random", parent = NULL) {
  cfg  <- P$cfg
  type <- cfg$strategy$type
  do_i <- type %in% c("install", "mixed")
  do_d <- type %in% c("decommission", "mixed")
  inst_budget  <- ls_limit(cfg$install$budget)
  decom_budget <- if (type == "mixed" && isTRUE(cfg$strategy$shared_budget)) inst_budget else ls_limit(cfg$decom$budget)

  empty_i <- list(cells = integer(0), n = numeric(0), cost = 0, budget = 0)
  empty_d <- list(cells = integer(0), cost = 0)
  d <- empty_d; i <- empty_i
  if (kind != "baseline_none") {
    keep_d <- integer(0); initial <- NULL
    if (kind == "evolutionary" && !is.null(parent)) {
      share <- stats::runif(1, ls_num(cfg$search$mutation_min, 0.1), ls_num(cfg$search$mutation_max, 0.4))
      if (length(parent$install)) {
        drop <- sample(seq_along(parent$install), ceiling(share * length(parent$install)))
        initial <- list(cells = parent$install[-drop], n = parent$modules[-drop])
      }
      keep_d <- parent$decom[stats::runif(length(parent$decom)) > share]
    }
    if (do_d) d <- ls_gen_decom(P, decom_budget, keep_d)
    if (do_i) {
      remaining <- if (type == "mixed" && isTRUE(cfg$strategy$shared_budget)) inst_budget - d$cost else inst_budget
      i <- ls_gen_modules(P, remaining, initial)
    }
  }
  cand <- list(kind = kind, install = i$cells, modules = i$n, decom = d$cells,
               install_cost = i$cost, decom_cost = d$cost, budget_drawn = i$budget)
  if (kind == "baseline_none") { cand$feasible <- TRUE; cand$note <- "" } else cand <- ls_check_candidate(P, cand)
  cand
}

# Habitat Layer Construction ----

ls_apply_candidate <- function(P, cand) {
  I <- P$cfg$install; D <- P$cfg$decom
  s    <- P$struct0
  habs <- P$hab0
  nat_sum <- function(idx) Reduce(`+`, lapply(habs, function(v) { x <- v[idx]; x[is.na(x)] <- 0; x }), rep(0, length(idx)))

  # Decommissioning: remove (part of) the structure and give the freed area back to natural habitat
  d <- cand$decom
  if (length(d)) {
    old <- s[d]
    s[d] <- old * (1 - ls_num(D$removal_fraction, 1))
    freed <- old - s[d]
    if (length(habs) && D$restore != "none") {
      if (D$restore %in% names(habs)) {
        habs[[D$restore]][d] <- habs[[D$restore]][d] + freed
      } else {
        N <- nat_sum(d)
        fallback <- names(habs)[which.max(vapply(habs, function(v) sum(v[d], na.rm = TRUE), numeric(1)))]
        for (h in names(habs)) {
          share <- ifelse(N > 0, habs[[h]][d] / N, as.numeric(h == fallback))
          habs[[h]][d] <- habs[[h]][d] + freed * share
        }
      }
    }
  }

  # Installation: modules raise the reef cover (Poisson overlap with halos amplified by reef
  # neighbours) and natural habitat is reduced to the room left in the cell
  chosen <- cand$install
  n_modules <- 0; mean_capacity <- 0
  if (length(chosen)) {
    cap <- ls_num(I$max_reef_fraction, 1)
    before <- s
    added <- numeric(length(s)); added[chosen] <- cand$modules
    rp <- ls_reef_metrics(P$depth[chosen], cand$modules, P$cell_area_m2[chosen], chosen,
                          (before > 0) | (added > 0), P$neighbors, I)
    s[chosen] <- ls_poisson_cover(before[chosen], rp$unit_fraction * rp$n_units, cap)
    reef_cov  <- pmax(0, pmin(cap, s[chosen]) - before[chosen])
    if (length(habs) && !identical(I$displacement, "none")) {
      room0 <- pmax(1e-12, 1 - before[chosen])
      resc  <- pmax(0, (room0 - reef_cov) / room0)
      for (h in names(habs)) habs[[h]][chosen] <- habs[[h]][chosen] * resc
    }
    n_modules <- sum(rp$n_units)
    mean_capacity <- mean(s[chosen])
  }
  s[is.na(P$depth)] <- NA

  # Spatial construction and connectivity metrics
  active <- !is.na(s) & s > 0
  contig <- if (any(active)) mean(vapply(which(active), function(i) sum(active[P$neighbors[[i]]]), numeric(1))) else 0
  list(structure = s, habitats = habs,
       metrics = list(n_modules = n_modules, mean_capacity = mean_capacity,
                      total_reef_cells = sum(active), mean_contiguity = contig,
                      footprint_km2 = sum(P$cell_area_m2[active]) / 1e6,
                      modules_per_cell = if (length(chosen)) n_modules / length(chosen) else 0))
}

# AAIGrid writes sidecar files (.prj, .asc.aux.xml) that Ecospace does not need
ls_cleanup_aux <- function(asc_path) {
  stem <- sub("\\.asc$", "", asc_path, ignore.case = TRUE)
  for (ext in c(".prj", ".asc.aux.xml", ".aux", ".asc.xml")) {
    f <- paste0(stem, ext)
    if (file.exists(f)) try(file.remove(f), silent = TRUE)
  }
}

ls_write_asc <- function(P, vals, path) {
  r <- rast(P$template)
  values(r) <- vals
  # Write to a temp dir and copy only the .asc so no sidecar files land next to the target
  tmp <- file.path(tempdir(), basename(path))
  suppressWarnings(writeRaster(r, tmp, filetype = "AAIGrid", overwrite = TRUE, NAflag = -9999))
  file.copy(tmp, path, overwrite = TRUE)
  stem <- sub("\\.asc$", "", tmp, ignore.case = TRUE)
  unlink(c(tmp, paste0(stem, c(".prj", ".asc.aux.xml", ".aux"))))
  invisible(path)
}

# Writes the run's maps to `map_dir`; returns named vector layer name -> file
ls_write_layers <- function(P, layers, map_dir) {
  dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)
  out <- character()
  f <- file.path(map_dir, basename(P$struct_file))
  ls_write_asc(P, layers$structure, f)
  out[P$cfg$layers$structure_layer] <- f
  for (h in names(layers$habitats)) {
    f <- file.path(map_dir, basename(P$layers$file[P$layers$name == h]))
    ls_write_asc(P, layers$habitats[[h]], f)
    out[h] <- f
  }
  out
}

# Command / STConfig Files ----

ls_set_tag <- function(lines, tag, value, default_type = "System.String") {
  hit <- grep(paste0("^\\s*", tag, "\\s*,"), lines)
  type <- default_type
  comment <- ""
  if (length(hit)) {
    old <- lines[hit[1]]
    pos <- regexpr("(System|Indexed)\\.[A-Za-z0-9.\\[\\]]+", old, perl = TRUE)
    if (pos > 0) {
      type <- regmatches(old, pos)
      comment <- trimws(sub("^\\s*,", "", substring(old, pos + attr(pos, "match.length"))))
    }
  }
  new <- paste0(tag, ", ", value, ", ", type, if (nzchar(comment)) paste0(", ", comment) else "")
  if (length(hit)) {
    lines[hit[1]] <- new
    if (length(hit) > 1) lines <- lines[-hit[-1]]
  } else {
    lines <- c(lines, new)
  }
  lines
}

ls_write_stconfig <- function(stconfig, layer_files, path) {
  doc <- read_xml(stconfig)
  xml_ns_strip(doc)
  for (nm in names(layer_files)) {
    xpath <- sprintf("//Configuration[translate(Name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')='%s']", tolower(nm))
    node <- xml_find_first(doc, xpath)
    if (!is.na(node)) {
      fn <- xml_find_first(node, "File")
      if (!is.na(fn)) xml_text(fn) <- ls_winpath(layer_files[[nm]])
    }
  }
  write_xml(doc, path)
  path
}

# Like structure_optimization.R, the command file is the region's template with the output folder
# set. The model and STConfig paths are pointed at this folder's copies (so it runs from any
# location), and run settings are written only when they differ from the template.
ls_write_cmd <- function(P, run_dir, stconfig_path) {
  M <- P$cfg$model
  lines <- readLines(M$cmd_template, warn = FALSE)
  tmpl  <- ls_read_cmd_tags(M$cmd_template)
  lines <- ls_set_tag(lines, "<ECOSPACE_OUTPUT_DIR>", ls_winpath(run_dir))
  lines <- ls_set_tag(lines, "<EWE_MODEL_FILE>", ls_winpath(M$model_file))
  lines <- ls_set_tag(lines, "<SPATIAL_CONFIG_FILE>", ls_winpath(stconfig_path))

  b <- function(x) if (isTRUE(x)) "True" else "False"
  th <- ls_num(M$n_threads, NA)
  settings <- list(
    "<ECOSIM_SCENARIO_INDEX>"     = ls_num(M$ecosim_scenario, NA),
    "<ECOSPACE_SCENARIO_INDEX>"   = ls_num(M$ecospace_scenario, NA),
    "<N_ECOSPACE_YEARS>"          = ls_num(M$n_years, NA),
    "<ECOSPACE_USE_SPINUP>"       = if (is.null(M$use_spinup)) NA else b(M$use_spinup),
    "<ECOSPACE_SPINUP_LENGTH>"    = ls_num(M$spinup_years, NA),
    "<N_ECOSPACE_TIMESTEPS_YEAR>" = ls_num(M$timesteps_year, NA),
    "<N_BIOMASS_THREADS>"         = th, "<N_GRID_THREADS>" = th,
    "<N_EFFORT_DIST_THREADS>"     = th, "<N_IBM_MOVEMENT_THREADS>" = th
  )
  same <- function(a, b) {
    if (is.na(b)) return(FALSE)
    na <- suppressWarnings(as.numeric(a)); nb <- suppressWarnings(as.numeric(b))
    if (!is.na(na) && !is.na(nb)) return(isTRUE(all.equal(na, nb)))
    identical(tolower(as.character(a)), tolower(as.character(b)))
  }
  for (tg in names(settings)) {
    v <- settings[[tg]]
    if (is.na(v)) next
    if (!same(v, unname(tmpl[tg]))) lines <- ls_set_tag(lines, tg, v)
  }
  extra <- trimws(unlist(strsplit(M$extra_tags %||% "", "\n")))
  extra <- extra[nzchar(extra)]
  for (ln in extra) {
    tg <- sub("^\\s*(<[^>]+>(\\([^)]*\\))?).*$", "\\1", ln)
    hit <- grep(paste0("^\\s*", gsub("([()])", "\\\\\\1", tg), "\\s*,"), lines)
    if (length(hit)) lines[hit[1]] <- ln else lines <- c(lines, ln)
  }
  cmd_file <- file.path(run_dir, "cmd.txt")
  writeLines(lines, cmd_file)
  cmd_file
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# Objective Readers ----

ls_read_ewe_csv <- function(run_dir, file_pattern) {
  hits <- list.files(run_dir, pattern = paste0(file_pattern, ".*\\.csv$"), recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(hits) == 0) return(NULL)
  lines <- tryCatch(readLines(hits[1], warn = FALSE), error = function(e) NULL)
  if (is.null(lines) || length(lines) == 0) return(NULL)

  # The header is the first row with the most fields (EwE writes metadata rows on top)
  stripped <- gsub('".*?"', '', lines)
  commas <- vapply(gregexpr(",", stripped), function(x) if (x[1] == -1) 0L else length(x), integer(1))
  if (max(commas) == 0) return(NULL)
  header_idx <- which(commas == max(commas))[1]
  tryCatch(read.csv(hits[1], skip = header_idx - 1, check.names = FALSE, stringsAsFactors = FALSE),
           error = function(e) NULL)
}

ls_split_targets <- function(x) {
  if (is.null(x) || all(is.na(x))) return(character())
  t <- trimws(unlist(strsplit(as.character(x), ",")))
  t[nzchar(t)]
}

# Objective values as in structure_optimization.R: means over the last N years; species objectives
# sum every column matching any target (and the fleet, if given); revenue = catch x lbs/t x $/lb x
# fleet multiplier over the whole model area; biodiversity = Shannon index of group biomass
ls_evaluate_objectives <- function(run_dir, objectives, last_n = 5, econ = ls_default_econ(), area_km2 = 1) {
  last_n <- max(1, round(ls_num(last_n, 5)))
  shannon <- function(row) {
    p <- row[!is.na(row) & row > 0]
    if (!length(p)) return(0)
    p <- p / sum(p)
    -sum(p * log(p))
  }
  prices <- econ$prices; fleets <- econ$fleets
  cache <- list()
  res <- list()
  for (k in seq_len(nrow(objectives))) {
    obj <- objectives[k, ]
    val <- NA_real_
    if (is.null(cache[[obj$file_pattern]])) cache[[obj$file_pattern]] <- ls_read_ewe_csv(run_dir, obj$file_pattern)
    df <- cache[[obj$file_pattern]]

    if (!is.null(df) && nrow(df) > 0 && ncol(df) > 1) {
      tv <- suppressWarnings(as.numeric(as.character(df[[1]])))
      rows <- if (any(is.finite(tv))) which(tv >= max(tv, na.rm = TRUE) - (last_n - 1)) else integer(0)
      if (!length(rows)) rows <- nrow(df)
      num <- suppressWarnings(matrix(as.numeric(as.character(unlist(df[, -1, drop = FALSE]))), nrow = nrow(df)))
      cols <- tolower(trimws(colnames(df)))
      fleet <- if (!is.null(obj$fleet) && !is.na(obj$fleet) && nzchar(obj$fleet)) tolower(trimws(obj$fleet)) else NA_character_

      if (obj$type == "sum") {
        val <- mean(rowSums(num, na.rm = TRUE)[rows], na.rm = TRUE)
      } else if (obj$type == "species") {
        hit <- integer(0)
        for (t in tolower(ls_split_targets(obj$target))) {
          m <- which(grepl(t, cols, fixed = TRUE))
          if (!is.na(fleet)) m <- m[grepl(fleet, cols[m], fixed = TRUE)]
          hit <- c(hit, m[m > 1] - 1)
        }
        hit <- unique(hit)
        if (length(hit)) val <- mean(rowSums(num[, hit, drop = FALSE], na.rm = TRUE)[rows], na.rm = TRUE)
      } else if (obj$type == "revenue") {
        total <- 0
        for (ci in seq_along(cols)[-1]) {
          cn <- cols[ci]
          if (!is.na(fleet) && !grepl(fleet, cn, fixed = TRUE)) next
          mult <- 1
          if (is.na(fleet) && !is.null(fleets) && nrow(fleets)) {
            f <- which(vapply(tolower(trimws(fleets$fleet)), grepl, logical(1), x = cn, fixed = TRUE))
            if (length(f)) mult <- as.numeric(fleets$multiplier[f[1]])
          }
          price <- ls_num(econ$default_price, 1)
          if (!is.null(prices) && nrow(prices)) {
            p <- which(vapply(tolower(trimws(prices$species)), grepl, logical(1), x = cn, fixed = TRUE))
            if (length(p)) price <- as.numeric(prices$price[p[1]])
          }
          avg <- mean(num[rows, ci - 1], na.rm = TRUE)
          if (is.finite(avg)) total <- total + avg * ls_num(econ$lbs_per_mt, 2204.62) * price * mult
        }
        val <- total * area_km2
      } else if (obj$type == "biodiversity") {
        val <- mean(apply(num, 1, shannon)[rows], na.rm = TRUE)
      }
    }
    if (!is.finite(val)) val <- NA_real_
    res[[obj$name]] <- val
  }
  res
}

# Dry run: placeholder objectives from the layout so the workflow can be tested without
# the Ecospace console. Values are NOT ecological predictions.
ls_synthetic_objectives <- function(P, layers, objectives) {
  s <- layers$structure; s[is.na(s)] <- 0
  x <- 10 * sum(s) / max(1, sum(P$water))
  dep <- if (any(s > 0)) mean(P$depth[s > 0], na.rm = TRUE) / max(P$depth, na.rm = TRUE) else 0
  res <- list()
  for (k in seq_len(nrow(objectives))) {
    a <- 0.4 + 0.3 * (k %% 3); b <- 0.15 + 0.1 * (k %% 2)
    res[[objectives$name[k]]] <- 100 * (1 + a * x - b * x^2 + 0.2 * dep * (k %% 2 - 0.5)) * (1 + stats::rnorm(1, 0, 0.01))
  }
  res
}

# Scoring ----

# Derived columns that ls_score() recomputes from scratch
ls_strip_scores <- function(df) {
  if (is.null(df)) return(NULL)
  derived <- c("Composite_Score", "Pareto", "Cost_Frontier", "Gross_Revenue_Gain", "Years_To_Benefit",
               "Lbs_Gained_Per_Module", "Lbs_Gained_Per_USD", "USD_Gained_Per_USD")
  drop <- names(df) %in% derived | grepl("_vs_status_quo_pct$|^Net_.*_Gain_(Lbs|USD)$", names(df))
  df[, !drop, drop = FALSE]
}

# As structure_optimization.R: each objective's change relative to the status quo, times its weight
# (sign flipped for "min"), summed into the composite score; the Pareto front uses the objectives
# with weight > 0. Absolute gains are converted to pounds (whole model area) and dollars.
ls_score <- function(df, objectives, area_km2 = 1, econ = ls_default_econ(), roi_objective = "RS_Catch",
                     revenue_objective = "Total_Revenue") {
  ok <- df$finished %in% TRUE & df$kind != "probe"   # a probe run read the wrong maps; never scored
  df$Composite_Score <- NA_real_
  df$Pareto <- FALSE
  df$Cost_Frontier <- FALSE
  sq <- which(df$kind == "baseline_none" & ok)
  sq <- if (length(sq)) sq[1] else NA_integer_
  lbs <- ls_num(econ$lbs_per_mt, 2204.62)

  comp <- rep(0, nrow(df))
  for (k in seq_len(nrow(objectives))) {
    obj <- objectives[k, ]
    v <- if (is.null(df[[obj$name]])) rep(NA_real_, nrow(df)) else suppressWarnings(as.numeric(df[[obj$name]]))
    b <- if (!is.na(sq)) v[sq] else NA_real_
    rel <- if (is.finite(b)) { if (b != 0) (v - b) / b else (v - b) } else rep(NA_real_, nrow(df))
    df[[paste0(obj$name, "_vs_status_quo_pct")]] <- 100 * rel
    w <- ls_num(obj$weight, 1)
    comp <- comp + ifelse(is.na(rel), 0, rel * w * (if (obj$direction == "min") -1 else 1))
    gain <- if (is.finite(b)) v - b else rep(NA_real_, nrow(df))
    if (obj$type %in% c("species", "sum")) df[[paste0("Net_", obj$name, "_Gain_Lbs")]] <- round(gain * area_km2 * lbs, 2)
    if (obj$type == "revenue") df[[paste0("Net_", obj$name, "_Gain_USD")]] <- round(gain, 2)
  }
  if (!is.na(sq)) df$Composite_Score[ok] <- comp[ok]

  # Reporting KPIs from the ROI and revenue objectives
  cost <- df$total_cost
  roi_lbs <- df[[paste0("Net_", roi_objective, "_Gain_Lbs")]]
  rev_gain <- df[[paste0("Net_", revenue_objective, "_Gain_USD")]]
  nmod <- if (is.null(df$n_modules)) rep(0, nrow(df)) else df$n_modules
  if (!is.null(roi_lbs)) {
    df$Lbs_Gained_Per_Module <- ifelse(nmod > 0, roi_lbs / nmod, 0)
    df$Lbs_Gained_Per_USD    <- ifelse(cost > 0, roi_lbs / cost, 0)
  }
  if (!is.null(rev_gain)) {
    df$Gross_Revenue_Gain <- rev_gain
    df$USD_Gained_Per_USD <- ifelse(cost > 0, rev_gain / cost, 0)
    df$Years_To_Benefit   <- ifelse(!is.na(rev_gain) & rev_gain > 0, cost / rev_gain, NA_real_)
  }

  # Pareto front over the weighted objectives (raw values, oriented so larger is better)
  scored <- objectives[suppressWarnings(as.numeric(objectives$weight)) > 0, , drop = FALSE]
  idx <- which(ok)
  if (length(idx) && nrow(scored)) {
    M <- sapply(seq_len(nrow(scored)), function(j) {
      v <- suppressWarnings(as.numeric(df[[scored$name[j]]][idx]))
      v[is.na(v)] <- -Inf
      if (scored$direction[j] == "min") -v else v
    })
    M <- matrix(M, nrow = length(idx))
    for (a in seq_along(idx)) {
      dominated <- FALSE
      for (b in seq_along(idx)) {
        if (a != b && all(M[b, ] >= M[a, ]) && any(M[b, ] > M[a, ])) { dominated <- TRUE; break }
      }
      df$Pareto[idx[a]] <- !dominated
    }
  }

  # Cost-efficiency frontier: no other run scores higher for the same or lower cost
  cand <- which(ok & is.finite(df$total_cost) & is.finite(df$Composite_Score))
  for (a in cand) {
    o <- setdiff(cand, a)
    df$Cost_Frontier[a] <- !any(df$Composite_Score[o] >= df$Composite_Score[a] & df$total_cost[o] <= df$total_cost[a] &
                                (df$Composite_Score[o] > df$Composite_Score[a] | df$total_cost[o] < df$total_cost[a]))
  }
  df
}

# Session Controller ----
# A session is an environment so the Shiny app can step it one run at a time.

ls_new_session <- function(cfg, repo_root) {
  S <- new.env()
  S$cfg <- cfg
  out_root <- file.path(repo_root, "Outputs", cfg$model$region)
  S$session_dir <- file.path(out_root, paste0("LiveSolver_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(S$session_dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.na(ls_num(cfg$search$seed))) set.seed(ls_num(cfg$search$seed))

  S$log <- character()
  ls_log(S, sprintf("Live Solver version %s", ls_version()))
  started <- FALSE
  on.exit(if (!started) {
    if (!is.null(S$restore)) for (k in seq_len(nrow(S$restore))) file.copy(S$restore$src[k], S$restore$dest[k], overwrite = TRUE)
    unlink(file.path(cfg$model$inputs_dir, ls_lock_name))
  }, add = TRUE)
  S$P <- ls_build_problem(cfg, S$session_dir)
  ls_save_config(cfg, file.path(S$session_dir, "solver_config.json"))
  if (!isTRUE(cfg$search$dry_run)) ls_save_settings(repo_root, cfg)

  S$total_runs <- round(ls_num(cfg$search$iterations, 100)) + 1   # + the status quo run
  S$i       <- 0
  S$results <- NULL
  S$cands   <- list()
  S$seen    <- character()
  S$results_file <- file.path(S$session_dir, "optimization_results.csv")
  S$closed  <- FALSE

  # Ecospace reads one fixed STConfig file and the maps it lists, so each run overwrites those maps.
  # The region's own STConfig is repaired in place to point at this folder's Inputs maps (its stored
  # paths may come from another computer). Everything touched is snapshotted, guarded by a lock for
  # crash recovery, and restored when the session ends.
  S$targets <- setNames(S$P$layers$file, S$P$layers$name)   # map files Ecospace reads, per layer
  if (!isTRUE(cfg$search$dry_run)) {
    stc <- normalizePath(cfg$model$stconfig, winslash = "/")
    file.copy(stc, file.path(S$P$snapshot_dir, basename(stc)), overwrite = TRUE)
    # Sidecars (.prj, .aux.xml) are removed while maps are overwritten (as in structure_optimization.R);
    # keep copies so the Inputs folder is left exactly as it was
    stems <- sub("\\.asc$", "", S$P$layers$file, ignore.case = TRUE)
    side  <- c(paste0(stems, ".prj"), paste0(stems, ".asc.aux.xml"))
    side  <- side[file.exists(side)]
    file.copy(side, file.path(S$P$snapshot_dir, basename(side)), overwrite = TRUE)
    keep  <- c(S$P$layers$file, stc, side)
    S$restore <- data.frame(dest = keep, src = file.path(S$P$snapshot_dir, basename(keep)),
                            stringsAsFactors = FALSE)
    ls_write_lock(S)
    S$stconfig <- ls_write_stconfig(stc, S$targets, stc)
    S$spatiallog <- file.path(dirname(cfg$model$model_file),
                              paste0(sub("\\.eweaccdb$", "", basename(cfg$model$model_file)), "_spatiallog.txt"))
    cd <- cfg$model$cache_dir %||% ""
    ls_check_cache_dir(cd, cfg)
    ls_log(S, if (nzchar(cd)) sprintf("Ecospace spatial cache folder: %s (emptied before every run)", cd) else
      "No Ecospace spatial cache folder set; if Ecospace loads layers from its cache, the session stops and says so.")
  }
  S$alerts <- character()
  S$alert_keys <- character()
  started <- TRUE
  ls_log(S, sprintf("Session %s | %s | strategy: %s | %d runs%s", basename(S$session_dir), basename(cfg$model$model_file),
                    cfg$strategy$type, S$total_runs, if (isTRUE(cfg$search$dry_run)) " | DRY RUN (synthetic objectives)" else ""))
  S
}

ls_write_lock <- function(S) {
  write_json(list(snapshot_dir = S$P$snapshot_dir, files = S$restore$dest, sources = S$restore$src),
             file.path(S$cfg$model$inputs_dir, ls_lock_name), auto_unbox = TRUE)
}

ls_log <- function(S, msg) {
  line <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)
  S$log <- c(S$log, line)
  message(line)
  cat(line, "\n", file = file.path(S$session_dir, "solver_log.txt"), append = TRUE)
}

# Best run so far (highest composite score)
ls_elite <- function(S) {
  if (is.null(S$results) || !any(is.finite(S$results$Composite_Score))) return(NULL)
  S$cands[[S$results$run[which.max(S$results$Composite_Score)]]]
}

# Parent for evolutionary mutation: a random run on the Pareto front that built something
# (structure_optimization.R mode 2)
ls_pick_parent <- function(S) {
  df <- S$results
  if (is.null(df)) return(NULL)
  front <- which(df$Pareto %in% TRUE)
  if (!length(front)) return(NULL)
  spent <- front[df$total_cost[front] > 0]
  pool <- if (length(spent)) spent else front
  S$cands[[df$run[if (length(pool) == 1) pool else sample(pool, 1)]]]
}

# Score with the session's objectives, economics and model area
ls_rescore <- function(S, df) {
  cfg <- S$cfg
  ls_score(ls_strip_scores(df), cfg$objectives, S$P$total_area_km2, cfg$econ,
           cfg$search$roi_objective %||% "RS_Catch", cfg$search$revenue_objective %||% "Total_Revenue")
}

ls_next_job <- function(S) {
  if (S$i >= S$total_runs) return(NULL)
  S$i <- S$i + 1
  idx <- S$i
  cfg <- S$cfg
  # Run 1 is the status quo; it is repeated if Ecospace turned out to read maps elsewhere (see ls_check_run)
  redo <- isTRUE(S$redo_status_quo)
  S$redo_status_quo <- FALSE
  kind <- if (idx == 1 || redo) "baseline_none" else {
    n_done <- sum(S$results$kind %in% c("random", "evolutionary"))
    if (cfg$search$algorithm == "evolutionary" && n_done >= ls_num(cfg$search$warmup, 0) && !is.null(ls_pick_parent(S))) "evolutionary" else "random"
  }
  cand <- NULL
  for (attempt in seq_len(max(1, ls_num(cfg$search$max_retries, 50)))) {
    cand <- ls_generate_candidate(S$P, kind, if (kind == "evolutionary") ls_pick_parent(S) else NULL)
    if (kind == "baseline_none") break
    if (cand$feasible && !ls_candidate_key(cand) %in% S$seen) break
  }
  run_name <- sprintf("Run_%04d%s", idx, if (kind == "baseline_none") "_Status_Quo" else "")
  S$seen <- c(S$seen, ls_candidate_key(cand))
  S$cands[[run_name]] <- cand
  list(idx = idx, run = run_name, kind = kind, cand = cand,
       run_dir = file.path(S$session_dir, run_name), started = Sys.time())
}

# Writes maps + command file and launches the console (or evaluates synthetically)
ls_start_job <- function(S, job) {
  P <- S$P; cfg <- S$cfg
  dir.create(job$run_dir, recursive = TRUE, showWarnings = FALSE)
  write_json(list(kind = job$kind, install = job$cand$install, modules = job$cand$modules, decom = job$cand$decom,
                  budget_drawn = job$cand$budget_drawn), file.path(job$run_dir, "candidate.json"))
  job$layers <- ls_apply_candidate(P, job$cand)
  job$map_files <- ls_write_layers(P, job$layers, file.path(job$run_dir, "Maps"))
  job$status <- NA_integer_

  if (!job$cand$feasible) {
    job$state <- "skipped"
    ls_log(S, sprintf("%s skipped: %s", job$run, job$cand$note))
    return(job)
  }
  if (isTRUE(cfg$search$dry_run)) {
    job$state <- "done"
    job$status <- 0L
    return(job)
  }

  # As structure_optimization.R: write each map straight over the active file in Inputs
  vals <- c(setNames(list(job$layers$structure), cfg$layers$structure_layer), job$layers$habitats)
  for (nm in names(job$map_files)) {
    target <- S$targets[[nm]]
    r <- rast(P$template)
    values(r) <- vals[[nm]]
    ok <- tryCatch({
      suppressWarnings(writeRaster(r, target, filetype = "AAIGrid", overwrite = TRUE, NAflag = -9999))
      TRUE
    }, error = function(e) FALSE)
    ls_cleanup_aux(target)
    if (!ok || !identical(unname(tools::md5sum(job$map_files[[nm]])), unname(tools::md5sum(target)))) {
      stop("Could not overwrite ", target, ". Is it open in another program?")
    }
  }
  # Ecospace caches converted habitat layers and reuses them instead of re-reading the files
  cd <- cfg$model$cache_dir %||% ""
  if (nzchar(cd) && dir.exists(cd)) unlink(list.files(cd, full.names = TRUE, all.files = TRUE, no.. = TRUE), recursive = TRUE, force = TRUE)
  cmd <- ls_write_cmd(P, job$run_dir, S$stconfig)

  console <- cfg$model$console_path
  if (!file.exists(console)) stop("Ecospace console not found: ", console, " (enable dry run to test without it)")
  job$proc <- processx::process$new(
    normalizePath(console), args = ls_winpath(cmd),
    stdout = file.path(job$run_dir, "console_stdout.log"),
    stderr = file.path(job$run_dir, "console_stderr.log"),
    cleanup_tree = TRUE, windows_hide_window = TRUE
  )
  job$state <- "running"
  job
}

# Returns the job with state "running" or "done"
ls_poll_job <- function(S, job) {
  if (job$state != "running") return(job)
  if (job$proc$is_alive()) {
    timeout <- ls_num(S$cfg$model$timeout_sec, 0)
    if (timeout > 0 && as.numeric(difftime(Sys.time(), job$started, units = "secs")) > timeout) {
      job$proc$kill_tree()
      job$status <- -1L
      job$state <- "done"
      ls_log(S, sprintf("%s timed out after %d s", job$run, timeout))
    }
    return(job)
  }
  job$status <- job$proc$get_exit_status()
  job$state <- "done"
  job
}

ls_finish_job <- function(S, job) {
  P <- S$P; cfg <- S$cfg; cand <- job$cand
  dry <- isTRUE(cfg$search$dry_run)
  finished <- job$state == "done" && (dry || length(list.files(job$run_dir, pattern = "Ecospace_.*\\.csv$", recursive = TRUE)) > 0)
  obj <- if (!finished) {
    setNames(as.list(rep(NA_real_, nrow(cfg$objectives))), cfg$objectives$name)
  } else if (dry) {
    ls_synthetic_objectives(P, job$layers, cfg$objectives)
  } else {
    ls_evaluate_objectives(job$run_dir, cfg$objectives, cfg$search$eval_last_n, cfg$econ, P$total_area_km2)
  }
  s <- job$layers$structure
  total_cost <- cand$install_cost + cand$decom_cost
  m <- job$layers$metrics
  row <- data.frame(
    run = job$run, iteration = job$idx, kind = job$kind,
    n_installed = length(cand$install), n_modules = m$n_modules, n_decommissioned = length(cand$decom),
    budget_drawn = round(ls_num(cand$budget_drawn, 0), 2),
    install_cost = round(cand$install_cost, 2), decom_cost = round(cand$decom_cost, 2),
    total_cost = round(total_cost, 2),
    Avg_Cost_Per_Module_USD = if (m$n_modules > 0) round(cand$install_cost / m$n_modules, 2) else 0,
    Modules_Per_Cell = round(m$modules_per_cell, 2), Mean_Contiguity_Index = round(m$mean_contiguity, 4),
    Total_Reef_Cells = m$total_reef_cells, Spatial_Footprint_km2 = round(m$footprint_km2, 2),
    mean_capacity = round(m$mean_capacity, 4),
    n_structures = sum(s > 0, na.rm = TRUE), structure_coverage = round(sum(s, na.rm = TRUE), 4),
    finished = finished, exit_status = job$status, dry_run = dry,
    runtime_sec = round(as.numeric(difftime(Sys.time(), job$started, units = "secs")), 1),
    map_dir = file.path(job$run_dir, "Maps"), timestamp = ls_timestamp(),
    stringsAsFactors = FALSE
  )
  row <- cbind(row, as.data.frame(obj, check.names = FALSE))
  write.csv(row, file.path(job$run_dir, "run_summary.csv"), row.names = FALSE)

  S$results <- ls_rescore(S, rbind(ls_strip_scores(S$results), row))
  write.csv(S$results, S$results_file, row.names = FALSE)

  sc <- S$results$Composite_Score[nrow(S$results)]
  ls_log(S, sprintf("%s (%s) | %s modules in %d cells, -%d structures | %s | %s | score %s",
                    job$run, job$kind, format(row$n_modules, big.mark = ","), row$n_installed, row$n_decommissioned, ls_usd(total_cost),
                    if (finished) "finished" else sprintf("FAILED (exit %s)", job$status),
                    if (is.na(sc)) "NA" else sprintf("%.3f", sc)))
  if (finished && !dry) {
    ls_check_run(S, job, obj)
    S$results <- ls_rescore(S, S$results)   # a run may have been relabelled
    write.csv(S$results, S$results_file, row.names = FALSE)
  }
  invisible(row)
}

# A user-entered cache folder is emptied before every run, so refuse anything that looks like a
# general-purpose folder or that holds the model, inputs or the Live Solver itself
ls_check_cache_dir <- function(cd, cfg) {
  if (!nzchar(cd)) return(invisible(TRUE))
  if (!dir.exists(cd)) stop("The Ecospace spatial cache folder does not exist: ", cd)
  np <- function(x) tolower(sub("/+$", "", normalizePath(x, winslash = "/", mustWork = FALSE)))
  c0 <- np(cd)
  home <- Sys.getenv(c("USERPROFILE", "HOME", "LOCALAPPDATA", "APPDATA", "TEMP", "TMP", "PROGRAMFILES"))
  home <- home[nzchar(home)]
  protected <- np(c(home, file.path(home, "Documents"), file.path(home, "Desktop"), file.path(home, "Downloads")))
  keep <- np(c(cfg$model$model_file, cfg$model$inputs_dir, cfg$model$console_path, getwd()))
  if (grepl("^[a-z]:$|^$", c0) || c0 %in% protected || any(startsWith(paste0(keep, "/"), paste0(c0, "/")))) {
    stop("Refusing to use '", cd, "' as the Ecospace spatial cache folder: its contents are deleted before every run, ",
         "and this looks like a general folder or contains the model or the Live Solver. Choose the cache folder itself.")
  }
  invisible(TRUE)
}

# Diagnostics after a real run: did Ecospace read this run's structure map, and did anything change?
# Logged every time; shown in the app once per kind of problem
ls_alert <- function(S, msg, key = msg) {
  ls_log(S, paste("WARNING:", msg))
  if (!key %in% S$alert_keys) {
    S$alert_keys <- c(S$alert_keys, key)
    S$alerts <- c(S$alerts, msg)
  }
}

ls_check_run <- function(S, job, obj) {
  norm_path <- function(x) tolower(gsub("\\\\", "/", x))
  log <- S$spatiallog
  loaded <- character()
  if (!is.null(log) && file.exists(log) && file.mtime(log) >= job$started - 5) {
    file.copy(log, file.path(job$run_dir, "spatiallog.txt"), overwrite = TRUE)
    log_lines <- readLines(log, warn = FALSE)
    loaded <- unique(sub(".*Loaded data from '(.*)'.*", "\\1", grep("Loaded data from '", log_lines, value = TRUE)))
    if (any(grepl("Loaded from cache", log_lines, fixed = TRUE))) {
      # This run used cached layers, not the solver's maps: never score it
      S$results$kind[S$results$run == job$run] <- "probe"
      cd <- S$cfg$model$cache_dir %||% ""
      ls_alert(S, sprintf("%s: Ecospace loaded the habitat maps from its spatial data cache instead of the files the solver wrote, so this run is not scored. %s",
                          job$run, if (nzchar(cd)) {
                            sprintf("The folder set as the cache (%s) was emptied before the run, so Ecospace's cache must be somewhere else.", cd)
                          } else {
                            "Set 'Ecospace spatial cache folder' on the Start here tab (it shows how to find the folder) and start again."
                          }), key = "cache")
      # Every later run would read the same cache, so stop rather than produce identical scenarios
      if (job$kind == "baseline_none") {
        S$total_runs <- S$i
        ls_log(S, "Stopped: the status quo run was served from Ecospace's cache.")
      }
      return(invisible())
    }
  } else {
    ls_alert(S, sprintf("%s: Ecospace wrote no spatial log (%s), so the solver cannot confirm which maps it read.", job$run, log),
             key = "no_spatiallog")
  }

  # Ecospace may read the maps listed in the STConfig saved with the model, not the one passed to it.
  # After the first run, retarget to the files it actually loaded (snapshot them, restore at the end)
  # and repeat the status quo run with the solver's maps.
  if (length(loaded) && !isTRUE(S$targets_checked)) {
    S$targets_checked <- TRUE
    moved <- character()
    for (nm in names(S$targets)) {
      hit <- loaded[tolower(basename(gsub("\\\\", "/", loaded))) == tolower(basename(S$targets[[nm]]))]
      if (!length(hit) || norm_path(hit[1]) == norm_path(S$targets[[nm]])) next
      ext <- if (.Platform$OS.type == "windows") hit[1] else gsub("\\\\", "/", hit[1])
      if (!file.exists(ext)) {
        ls_alert(S, sprintf("Ecospace reads the %s map from %s, which this computer cannot open. Re-save the model's spatial configuration to point at this folder's Inputs maps.", nm, ext))
        next
      }
      snap <- file.path(S$P$snapshot_dir, "external", nm, basename(ext))
      dir.create(dirname(snap), recursive = TRUE, showWarnings = FALSE)
      file.copy(ext, snap, overwrite = TRUE)
      S$restore <- rbind(S$restore, data.frame(dest = ext, src = snap, stringsAsFactors = FALSE))
      S$targets[[nm]] <- ext
      moved <- c(moved, ext)
    }
    if (length(moved)) {
      ls_write_lock(S)
      ls_alert(S, sprintf("Ecospace reads its maps from %s (the spatial configuration saved with the model), not from this folder. The solver now updates those files before each run and restores them afterwards. The status quo run is repeated with the solver's maps.",
                          paste(unique(dirname(moved)), collapse = ", ")))
      S$results$kind[S$results$run == job$run] <- "probe"
      S$redo_status_quo <- TRUE
      S$total_runs <- S$total_runs + 1
      return(invisible())
    }
  }
  if (length(loaded) && !norm_path(S$targets[[S$cfg$layers$structure_layer]]) %in% norm_path(loaded)) {
    ls_alert(S, sprintf("%s: Ecospace did not load %s. Its spatial log lists: %s", job$run,
                        S$targets[[S$cfg$layers$structure_layer]], paste(unique(loaded), collapse = ", ")), key = "wrong_map")
  }
  sq <- S$results[S$results$kind == "baseline_none" & S$results$finished %in% TRUE & S$results$run != job$run, , drop = FALSE]
  changed <- length(job$cand$install) + length(job$cand$decom) > 0
  if (job$kind != "baseline_none" && changed && nrow(sq)) {
    same <- vapply(names(obj), function(n) isTRUE(all.equal(as.numeric(obj[[n]]), as.numeric(sq[[n]][1]), tolerance = 0)), logical(1))
    if (all(same)) {
      ls_alert(S, sprintf("%s changed %d cells but every outcome is identical to the status quo; Ecospace may not be reading the new maps.",
                          job$run, length(job$cand$install) + length(job$cand$decom)), key = "identical")
    }
  }
}

# Kill any running console, put pristine maps back and remove the lock
ls_close_session <- function(S, job = NULL) {
  if (isTRUE(S$closed)) return(invisible(NULL))
  if (!is.null(job$proc) && job$proc$is_alive()) job$proc$kill_tree()
  cfg <- S$cfg
  if (!isTRUE(cfg$search$dry_run)) {
    for (k in seq_len(nrow(S$restore))) file.copy(S$restore$src[k], S$restore$dest[k], overwrite = TRUE)
    unlink(file.path(cfg$model$inputs_dir, ls_lock_name))
    ls_log(S, "Restored the original maps and STConfig in the Inputs folder.")
  }
  if (!is.null(S$results) && any(is.finite(S$results$Composite_Score))) {
    best <- S$results[which.max(S$results$Composite_Score), ]
    best_dir <- file.path(S$session_dir, "Best_Maps")
    dir.create(best_dir, showWarnings = FALSE)
    file.copy(list.files(best$map_dir, full.names = TRUE), best_dir, overwrite = TRUE)
    ls_log(S, sprintf("Best run: %s | score %.3f | cost %s", best$run, best$Composite_Score, ls_usd(best$total_cost)))
  }
  S$closed <- TRUE
  invisible(NULL)
}

# Blocking loop used by run_headless.R
ls_run_session <- function(cfg, repo_root, on_progress = NULL) {
  S <- ls_new_session(cfg, repo_root)
  job <- NULL
  on.exit(ls_close_session(S, job), add = TRUE)
  while (!is.null(job <- ls_next_job(S))) {
    job <- ls_start_job(S, job)
    while (job$state == "running") {
      Sys.sleep(1)
      job <- ls_poll_job(S, job)
    }
    ls_finish_job(S, job)
    if (is.function(on_progress)) on_progress(S)
  }
  job <- NULL
  S
}

# Plotting ----

# Layout map: structure cells are shaded by the proportion of the cell that is structure
# (the value Ecospace reads), on a land / water / eligible-for-installation background.
# New and decommissioned cells are outlined so changes stay visible at any cover.
ls_cover_breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
ls_cover_colors <- c("#86b6ef", "#3987e5", "#256abf", "#184f95", "#0d366b")
ls_bg_colors    <- c(land = "#d9d9d9", water = "#eef3f8", eligible = "#d4ecd0")
ls_new_col      <- "#eb6834"
ls_removed_col  <- "#d55181"
ls_decom_ok_col <- "#8a8a8a"

ls_plot_layout <- function(P, cand = NULL, main = "", show_eligible = TRUE) {
  e  <- as.vector(ext(P$template))
  dx <- (e[2] - e[1]) / P$nc
  dy <- (e[4] - e[3]) / P$nr
  xs <- seq(e[1], e[2], length.out = P$nc + 1)
  ys <- seq(e[3], e[4], length.out = P$nr + 1)
  grid <- function(v) t(matrix(v, nrow = P$nr, ncol = P$nc, byrow = TRUE)[P$nr:1, , drop = FALSE])

  bg <- ifelse(P$water, 2, 1)
  if (show_eligible) bg[P$install_valid] <- 3
  cover <- if (is.null(cand)) P$struct0 else ls_apply_candidate(P, cand)$structure
  cover[is.na(cover) | cover <= 0 | !P$water] <- NA
  cover <- pmin(cover, 1)

  op <- par(mar = c(3.4, 3, 2, 1), mgp = c(1.8, 0.5, 0))
  on.exit(par(op))
  # Shrink the plot region to the map's true shape (instead of asp, which pads with blank space)
  ratio <- diff(e[3:4]) / (diff(e[1:2]) * cos(mean(e[3:4]) * pi / 180))
  pin <- par("pin")
  par(pin = if (pin[1] * ratio <= pin[2]) c(pin[1], pin[1] * ratio) else c(pin[2] / ratio, pin[2]))
  image(xs, ys, grid(bg), col = ls_bg_colors, breaks = c(0.5, 1.5, 2.5, 3.5),
        xlab = "Longitude", ylab = "Latitude", main = main, useRaster = TRUE, xaxs = "i", yaxs = "i")
  if (any(!is.na(cover))) {
    image(xs, ys, grid(cover), col = ls_cover_colors, breaks = ls_cover_breaks, add = TRUE, useRaster = TRUE)
  }

  outline <- function(idx, col, lwd = 1.6, lty = 1, cross = FALSE) {
    if (!length(idx)) return(invisible())
    r <- (idx - 1) %/% P$nc; c <- (idx - 1) %% P$nc
    x0 <- e[1] + c * dx; y1 <- e[4] - r * dy
    rect(x0, y1 - dy, x0 + dx, y1, border = col, lwd = lwd, lty = lty)
    if (cross) segments(c(x0, x0), c(y1 - dy, y1), c(x0 + dx, x0 + dx), c(y1, y1 - dy), col = col, lwd = 1)
  }
  if (show_eligible) outline(setdiff(P$decom_eligible, cand$decom), ls_decom_ok_col, lwd = 1, lty = 2)
  outline(cand$install, ls_new_col)
  outline(cand$decom, ls_removed_col, cross = TRUE)

  # Legend: background, then cover classes present, then change outlines
  lab <- c("Land", "Water"); fill <- ls_bg_colors[1:2]; border <- rep("grey55", 2)
  if (show_eligible && length(P$install_valid)) {
    lab <- c(lab, "Eligible: install"); fill <- c(fill, ls_bg_colors[3]); border <- c(border, "grey55")
  }
  if (any(!is.na(cover))) {
    used <- sort(unique(cut(cover[!is.na(cover)], ls_cover_breaks, labels = FALSE, include.lowest = TRUE)))
    lab <- c(lab, sprintf("Structure cover %.0f-%.0f%%", 100 * ls_cover_breaks[used], 100 * ls_cover_breaks[used + 1]))
    fill <- c(fill, ls_cover_colors[used]); border <- c(border, ls_cover_colors[used])
  }
  if (show_eligible && length(setdiff(P$decom_eligible, cand$decom))) {
    lab <- c(lab, "Eligible: decommission"); fill <- c(fill, NA); border <- c(border, ls_decom_ok_col)
  }
  if (length(cand$install)) { lab <- c(lab, "New structure"); fill <- c(fill, NA); border <- c(border, ls_new_col) }
  if (length(cand$decom))   { lab <- c(lab, "Decommissioned"); fill <- c(fill, NA); border <- c(border, ls_removed_col) }
  legend("bottomright", legend = lab, fill = fill, border = border, cex = 0.72, bg = "white", box.col = "grey70")
}
