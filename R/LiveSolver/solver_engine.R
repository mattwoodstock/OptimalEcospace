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

ls_default_objectives <- function() {
  data.frame(
    name         = c("RS_Biomass", "Tot_Biomass", "RS_Catch", "Tot_Catch", "Biodiversity", "Lionfish_Biomass"),
    file_pattern = c("Annual_Average_Biomass", "Annual_Average_Biomass", "Annual_Average_Catch",
                     "Annual_Average_Catch", "Annual_Average_Biomass", "Annual_Average_Biomass"),
    target       = c("red snapper adult", NA, "red snapper adult", NA, NA, "lionfish"),
    type         = c("species", "sum", "species", "sum", "biodiversity", "species"),
    direction    = c("max", "max", "max", "max", "max", "min"),
    weight       = c(1.0, 0.5, 2.0, 0.5, 1.0, 1.0),
    stringsAsFactors = FALSE
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
  console <- ls_find_console(repo_root)

  list(
    model = list(
      region            = if (!is.null(m)) m$region else NA,
      model_file        = if (!is.null(m)) m$model_file else NA,
      inputs_dir        = if (!is.null(m)) m$inputs_dir else NA,
      stconfig          = if (!is.null(info)) info$stconfig else NA,
      cmd_template      = if (!is.null(info)) info$cmd_template else NA,
      console_path      = console,
      ecosim_scenario   = 1,
      ecospace_scenario = 1,
      n_years           = 30,
      use_spinup        = TRUE,
      spinup_years      = 5,
      timesteps_year    = 12,
      n_threads         = 15,
      timeout_sec       = 1800,
      extra_tags        = ""      # raw command-file lines appended verbatim (e.g. vulnerabilities)
    ),
    layers = list(
      structure_layer   = struct_layer,
      existing_file     = "",    # "" = use the pristine structure layer as existing structures
      natural_habitats  = if (!is.null(lyr)) setdiff(lyr$name, struct_layer) else character(),
      basemap_file      = "basemap.asc",
      pristine_source   = "inputs" # "inputs" (current Inputs files) or "backup" (Backup_Base_Maps)
    ),
    strategy = list(
      type          = "install",  # install | decommission | mixed
      shared_budget = FALSE        # mixed only: decommissioning spends from the installation budget
    ),
    install = list(
      budget                = 500000,
      fill_budget           = TRUE,   # TRUE: keep adding until budget/max reached; FALSE: random count in [min_n, max_n]
      min_n                 = 0,
      max_n                 = NA,
      coverage_fraction     = 1.0,
      cost_source           = "map",  # map | depth | fixed
      cost_map_file         = "cost_map.asc",
      cost_multiplier       = 1,
      fixed_cost            = 0,      # per structure, added to every cost source
      depth_cost_rate       = 1000,   # USD per metre of depth (cost_source = "depth")
      scale_cost_by_coverage = FALSE,
      depth_min             = NA,
      depth_max             = NA,
      use_exclusions        = TRUE,
      exclusions_file       = "exclusions.asc",
      extra_mask_file       = "",     # optional .asc; cells > 0 are allowed
      xmin = NA, xmax = NA, ymin = NA, ymax = NA,
      min_spacing           = 0,      # cells between new structures (Chebyshev)
      buffer_existing       = 0,      # cells kept clear around existing structures
      avoid_habitats        = character(),
      avoid_threshold       = 0.5,    # skip cells where avoided habitats sum above this
      placement             = "random", # random | clustered | cost_weighted | depth_weighted
      n_clusters            = 3,
      cluster_radius        = 3,
      displacement          = "proportional" # proportional | subtract | none
    ),
    decom = list(
      budget            = NA,
      min_n             = 0,
      max_n             = NA,
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
      algorithm         = "random",   # random | evolutionary
      warmup            = 20,
      mutation_rate     = 0.15,
      include_baselines = TRUE,
      seed              = NA,
      map_mode          = "overwrite", # overwrite | isolated
      dry_run           = !file.exists(console),
      eval_last_n       = 1,           # average objectives over the last N output rows
      roi_objective     = "RS_Catch",
      max_retries       = 50
    ),
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
  for (sec in setdiff(names(base), "objectives")) {
    if (!is.null(raw[[sec]])) {
      for (k in names(raw[[sec]])) {
        v <- raw[[sec]][[k]]
        base[[sec]][[k]] <- if (is.null(v)) NA else v
      }
    }
  }
  if (!is.null(raw$objectives)) base$objectives <- as.data.frame(raw$objectives, stringsAsFactors = FALSE)
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
      src <- file.path(info$snapshot_dir, basename(info$files[i]))
      if (file.exists(src)) file.copy(src, info$files[i], overwrite = TRUE)
    }
    message("Recovered pristine Inputs maps from interrupted session: ", info$snapshot_dir)
  }
  file.remove(lock)
  TRUE
}

# Everything that stays fixed for a whole session
# recover = FALSE for previews: never touch the Inputs folder while a session may be running
ls_build_problem <- function(cfg, session_dir = NULL, recover = TRUE) {
  M <- cfg$model; L <- cfg$layers
  inputs_dir <- M$inputs_dir
  if (is.na(inputs_dir) || !dir.exists(inputs_dir)) stop("Inputs directory not found: ", inputs_dir)
  recovered <- recover && ls_recover_inputs(inputs_dir)

  layers <- ls_read_stconfig(M$stconfig, inputs_dir)
  if (!L$structure_layer %in% layers$name) stop("Structure layer '", L$structure_layer, "' is not in the STConfig file.")
  natural <- intersect(L$natural_habitats, layers$name)
  natural <- setdiff(natural, L$structure_layer)
  used    <- layers[layers$name %in% c(L$structure_layer, natural), ]
  missing <- used$name[!used$exists]
  if (length(missing)) stop("Layer file(s) missing in Inputs: ", paste(missing, collapse = ", "))

  basemap_file <- ls_resolve(L$basemap_file, inputs_dir)
  if (is.na(basemap_file)) stop("Basemap not found: ", L$basemap_file)
  template <- rast(basemap_file)
  nr <- nrow(template); nc <- ncol(template)
  depth <- values(template, mat = FALSE)
  water <- !is.na(depth) & depth > 0
  xy    <- xyFromCell(template, seq_len(ncell(template)))

  # Pristine source: current Inputs files or the legacy Backup_Base_Maps folder
  pristine_file <- function(f) {
    if (identical(L$pristine_source, "backup")) {
      b <- file.path(inputs_dir, "Backup_Base_Maps", basename(f))
      if (file.exists(b)) return(b)
    }
    f
  }
  snapshot_dir <- NULL
  if (!is.null(session_dir)) {
    snapshot_dir <- file.path(session_dir, "_pristine")
    dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)
    for (f in used$file) file.copy(pristine_file(f), file.path(snapshot_dir, basename(f)), overwrite = TRUE)
  }
  src <- function(f) if (!is.null(snapshot_dir)) file.path(snapshot_dir, basename(f)) else pristine_file(f)

  struct_file <- used$file[used$name == L$structure_layer]
  struct0 <- ls_read_layer(src(struct_file), template)
  struct0[is.na(struct0)] <- 0
  hab0 <- setNames(lapply(natural, function(h) ls_read_layer(src(used$file[used$name == h]), template, "bilinear")), natural)

  existing_f <- ls_resolve(L$existing_file, inputs_dir)
  existing <- if (!is.na(existing_f)) ls_read_layer(existing_f, template) else struct0
  existing[is.na(existing)] <- 0

  # Installation feasibility ----
  I <- cfg$install
  inst_ok <- water & existing == 0 & struct0 == 0
  if (isTRUE(I$use_exclusions)) {
    ef <- ls_resolve(I$exclusions_file, inputs_dir)
    if (is.na(ef)) stop("Exclusions map not found: ", I$exclusions_file)
    ex <- ls_read_layer(ef, template)
    inst_ok <- inst_ok & !is.na(ex) & ex > 0
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
    hard <- Reduce(`+`, lapply(hab0[avoid], function(v) { v[is.na(v)] <- 0; v }))
    inst_ok <- inst_ok & hard <= ls_num(I$avoid_threshold, 1)
  }
  inst_cost <- ls_cost_vector(I, depth, inputs_dir, template)
  if (isTRUE(I$scale_cost_by_coverage)) inst_cost <- inst_cost * ls_num(I$coverage_fraction, 1)

  # Decommissioning feasibility ----
  D <- cfg$decom
  dec_ok <- existing > ls_num(D$eligible_threshold, 0) & struct0 > 0 &
    ls_depth_mask(D, depth) & ls_bbox_mask(D, xy)
  dec_cost <- ls_cost_vector(D, depth, inputs_dir, template)

  type <- cfg$strategy$type
  P <- list(
    cfg = cfg, session_dir = session_dir, snapshot_dir = snapshot_dir, recovered = recovered,
    template = template, nr = nr, nc = nc, depth = depth, water = water, xy = xy,
    layers = used, struct_file = struct_file, natural = natural,
    struct0 = struct0, hab0 = hab0, existing = existing,
    install_valid  = if (type %in% c("install", "mixed")) which(inst_ok) else integer(0),
    decom_eligible = if (type %in% c("decommission", "mixed")) which(dec_ok) else integer(0),
    install_cost = inst_cost, decom_cost = dec_cost
  )
  P$summary <- ls_problem_summary(P)
  P
}

ls_problem_summary <- function(P) {
  cfg <- P$cfg
  ic <- P$install_cost[P$install_valid]
  dc <- P$decom_cost[P$decom_eligible]
  data.frame(
    item = c("Grid (rows x cols)", "Water cells", "Existing structure cells",
             "Cells eligible for installation", "Installation cost range",
             "Max installs within budget (cheapest-first)",
             "Structures eligible for decommissioning", "Removal cost range"),
    value = c(
      sprintf("%d x %d", P$nr, P$nc), sum(P$water), sum(P$existing > 0),
      length(P$install_valid),
      if (length(ic)) paste(ls_usd(min(ic)), "-", ls_usd(max(ic))) else "-",
      if (length(ic)) sum(cumsum(sort(ic)) <= ls_limit(cfg$install$budget)) else 0,
      length(P$decom_eligible),
      if (length(dc)) paste(ls_usd(min(dc)), "-", ls_usd(max(dc))) else "-"
    ),
    stringsAsFactors = FALSE
  )
}

# Candidate Generation ----

ls_install_order <- function(P, pool) {
  I <- P$cfg$install
  switch(I$placement,
    cost_weighted  = ls_shuffle(pool, 1 / pmax(P$install_cost[pool], 1)),
    depth_weighted = ls_shuffle(pool, P$depth[pool]),
    clustered = {
      k <- max(1, min(length(pool), round(ls_num(I$n_clusters, 1))))
      centers <- ls_shuffle(pool)[seq_len(k)]
      near <- ls_within(centers, max(0, round(ls_num(I$cluster_radius, 0))), P$nr, P$nc)
      ls_shuffle(pool[near[pool]])
    },
    ls_shuffle(pool)
  )
}

# Greedy fill: walk `order`, keep cells that respect budget and spacing
ls_greedy <- function(P, order, cost, budget, target, spacing) {
  blocked <- logical(P$nr * P$nc)
  chosen  <- integer(0)
  spent   <- 0
  min_c   <- if (length(order)) min(cost[order]) else 0
  for (i in order) {
    if (length(chosen) >= target || spent + min_c > budget) break
    if (blocked[i] || spent + cost[i] > budget) next
    chosen <- c(chosen, i)
    spent  <- spent + cost[i]
    if (spacing > 0) blocked[ls_neighbors(i, spacing, P$nr, P$nc)] <- TRUE else blocked[i] <- TRUE
  }
  list(cells = chosen, cost = spent)
}

ls_gen_install <- function(P, budget, keep = integer(0), target = NULL, ignore_budget = FALSE) {
  I <- P$cfg$install
  pool <- P$install_valid
  if (!length(pool)) return(list(cells = integer(0), cost = 0))
  max_n <- min(length(pool), ls_limit(I$max_n))
  min_n <- ls_num(I$min_n, 0)
  if (is.null(target)) {
    target <- if (isTRUE(I$fill_budget) || ignore_budget) max_n else ls_sample_between(min_n, max_n)
  }
  if (is.na(target)) return(list(cells = integer(0), cost = 0, infeasible = TRUE))
  keep  <- intersect(keep, pool)
  order <- c(ls_shuffle(keep), setdiff(ls_install_order(P, pool), keep))
  # Clustered placement may cover few cells; top up with the rest of the pool
  if (I$placement == "clustered" && isTRUE(I$fill_budget)) order <- c(order, ls_shuffle(setdiff(pool, order)))
  ls_greedy(P, order, P$install_cost, if (ignore_budget) Inf else budget, target,
            max(0, round(ls_num(I$min_spacing, 0))))
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

ls_gen_decom <- function(P, budget, keep = integer(0), target = NULL, ignore_budget = FALSE) {
  D <- P$cfg$decom
  pool <- P$decom_eligible
  if (!length(pool)) return(list(cells = integer(0), cost = 0))
  n_exist <- sum(P$existing > 0)
  max_n <- min(length(pool), ls_limit(D$max_n), n_exist - ls_num(D$min_retained, 0))
  if (ignore_budget) target <- length(pool)
  if (is.null(target)) target <- ls_sample_between(ls_num(D$min_n, 0), max_n)
  if (is.na(target)) return(list(cells = integer(0), cost = 0, infeasible = TRUE))
  keep  <- intersect(keep, pool)
  order <- c(ls_shuffle(keep), setdiff(ls_decom_order(P, pool), keep))
  ls_greedy(P, order, P$decom_cost, if (ignore_budget) Inf else budget, target, 0)
}

ls_candidate_key <- function(cand) {
  paste(paste(sort(cand$install), collapse = "."), paste(sort(cand$decom), collapse = "."), sep = "|")
}

ls_check_candidate <- function(P, cand) {
  type <- P$cfg$strategy$type
  notes <- character()
  if (type %in% c("install", "mixed") && length(cand$install) < ls_num(P$cfg$install$min_n, 0))
    notes <- c(notes, "fewer installs than the minimum")
  if (type %in% c("decommission", "mixed") && length(cand$decom) < ls_num(P$cfg$decom$min_n, 0))
    notes <- c(notes, "fewer removals than the minimum")
  cand$feasible <- !length(notes)
  cand$note <- paste(notes, collapse = "; ")
  cand
}

# kind: baseline_none | baseline_all | random | evolutionary
ls_generate_candidate <- function(P, kind = "random", elite = NULL) {
  cfg  <- P$cfg
  type <- cfg$strategy$type
  do_i <- type %in% c("install", "mixed")
  do_d <- type %in% c("decommission", "mixed")
  inst_budget  <- ls_limit(cfg$install$budget)
  decom_budget <- if (type == "mixed" && isTRUE(cfg$strategy$shared_budget)) inst_budget else ls_limit(cfg$decom$budget)

  empty <- list(cells = integer(0), cost = 0)
  if (kind == "baseline_none") {
    d <- empty; i <- empty
  } else if (kind == "baseline_all") {
    d <- if (do_d) ls_gen_decom(P, Inf, ignore_budget = TRUE) else empty
    i <- if (do_i) ls_gen_install(P, Inf, ignore_budget = TRUE) else empty
  } else {
    rate <- ls_num(cfg$search$mutation_rate, 0.15)
    mutate_keep <- function(x) x[runif(length(x)) > rate]
    jitter <- function(n, lo, hi) {
      n <- n + sample(-max(1, round(rate * n)):max(1, round(rate * n)), 1)
      max(lo, min(hi, n))
    }
    keep_d <- if (kind == "evolutionary" && !is.null(elite)) mutate_keep(elite$decom) else integer(0)
    keep_i <- if (kind == "evolutionary" && !is.null(elite)) mutate_keep(elite$install) else integer(0)

    d <- empty
    if (do_d) {
      tgt <- NULL
      if (kind == "evolutionary" && !is.null(elite)) {
        hi <- min(length(P$decom_eligible), ls_limit(cfg$decom$max_n), sum(P$existing > 0) - ls_num(cfg$decom$min_retained, 0))
        tgt <- jitter(length(elite$decom), ls_num(cfg$decom$min_n, 0), hi)
      }
      d <- ls_gen_decom(P, decom_budget, keep_d, tgt)
    }
    i <- empty
    if (do_i) {
      remaining <- if (type == "mixed" && isTRUE(cfg$strategy$shared_budget)) inst_budget - d$cost else inst_budget
      tgt <- NULL
      if (kind == "evolutionary" && !is.null(elite) && !isTRUE(cfg$install$fill_budget)) {
        hi <- min(length(P$install_valid), ls_limit(cfg$install$max_n))
        tgt <- jitter(length(elite$install), ls_num(cfg$install$min_n, 0), hi)
      }
      i <- ls_gen_install(P, remaining, keep_i, tgt)
    }
  }
  cand <- list(kind = kind, install = i$cells, decom = d$cells,
               install_cost = i$cost, decom_cost = d$cost)
  if (kind %in% c("random", "evolutionary")) cand <- ls_check_candidate(P, cand) else { cand$feasible <- TRUE; cand$note <- "" }
  cand
}

# Habitat Layer Construction ----

ls_apply_candidate <- function(P, cand) {
  I <- P$cfg$install; D <- P$cfg$decom
  s    <- P$struct0
  habs <- lapply(P$hab0, function(v) { v[is.na(v) & P$water] <- 0; v })
  nat_sum <- function(idx) Reduce(`+`, lapply(habs, `[`, idx), rep(0, length(idx)))

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

  # Installation: add structure and displace natural habitat
  i <- cand$install
  if (length(i)) {
    cov <- ls_num(I$coverage_fraction, 1)
    added <- pmax(0, pmin(1, s[i] + cov) - s[i])
    s[i] <- s[i] + added
    if (length(habs)) {
      if (I$displacement == "proportional") {
        N <- nat_sum(i)
        f <- ifelse(N > 0, pmax(0, N - added) / N, 1)
        for (h in names(habs)) habs[[h]][i] <- habs[[h]][i] * f
      } else if (I$displacement == "subtract") {
        for (h in names(habs)) habs[[h]][i] <- pmax(0, habs[[h]][i] - added)
      }
    }
  }

  s[is.na(P$depth)] <- NA
  list(structure = s, habitats = habs)
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

ls_write_cmd <- function(P, run_dir, stconfig_path) {
  M <- P$cfg$model
  lines <- readLines(M$cmd_template, warn = FALSE)
  b <- function(x) if (isTRUE(x)) "True" else "False"
  tags <- list(
    "<EWE_MODEL_FILE>"              = ls_winpath(M$model_file),
    "<SPATIAL_CONFIG_FILE>"         = ls_winpath(stconfig_path),
    "<ECOSIM_SCENARIO_INDEX>"       = ls_num(M$ecosim_scenario, 1),
    "<ECOSPACE_SCENARIO_INDEX>"     = ls_num(M$ecospace_scenario, 1),
    "<N_ECOSPACE_YEARS>"            = ls_num(M$n_years, 30),
    "<ECOSPACE_USE_SPINUP>"         = b(M$use_spinup),
    "<ECOSPACE_SPINUP_LENGTH>"      = ls_num(M$spinup_years, 5),
    "<N_ECOSPACE_TIMESTEPS_YEAR>"   = ls_num(M$timesteps_year, 12),
    "<N_BIOMASS_THREADS>"           = ls_num(M$n_threads, 15),
    "<N_GRID_THREADS>"              = ls_num(M$n_threads, 15),
    "<N_EFFORT_DIST_THREADS>"       = ls_num(M$n_threads, 15),
    "<N_IBM_MOVEMENT_THREADS>"      = ls_num(M$n_threads, 15),
    "<ECOSPACE_USE_CORE_OUPUT_DIR>" = "False",
    "<ECOSPACE_OUTPUT_DIR>"         = ls_winpath(run_dir),
    "<ECOSPACE_USE_ANNUAL_OUTPUT>"  = "True",
    "<ECOSPACE_SAVE_AVERAGED_BIOMASS>" = "True"
  )
  for (tg in names(tags)) lines <- ls_set_tag(lines, tg, tags[[tg]])
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

ls_evaluate_objectives <- function(run_dir, objectives, last_n = 1) {
  last_n <- max(1, round(ls_num(last_n, 1)))
  tail_mean <- function(x) mean(tail(x, last_n), na.rm = TRUE)
  shannon <- function(row) {
    p <- row[!is.na(row) & row > 0]
    if (!length(p)) return(0)
    p <- p / sum(p)
    -sum(p * log(p))
  }
  cache <- list()
  res <- list()
  for (k in seq_len(nrow(objectives))) {
    obj <- objectives[k, ]
    val <- NA_real_
    if (is.null(cache[[obj$file_pattern]])) cache[[obj$file_pattern]] <- ls_read_ewe_csv(run_dir, obj$file_pattern)
    df <- cache[[obj$file_pattern]]

    if (!is.null(df) && nrow(df) > 0 && ncol(df) > 1) {
      is_ts <- grepl("time|year|step", tolower(trimws(colnames(df)[1])))
      num <- suppressWarnings(matrix(as.numeric(as.character(unlist(df[, -1, drop = FALSE]))), nrow = nrow(df)))
      target <- tolower(trimws(obj$target))
      cols <- tolower(trimws(colnames(df)))
      col_match <- if (!is.na(target) && nzchar(target)) {
        m <- which(cols == target)
        if (!length(m)) m <- which(grepl(target, cols, fixed = TRUE))
        m[m > 1] - 1
      } else integer(0)

      if (obj$type == "sum") {
        val <- if (is_ts) tail_mean(rowSums(num, na.rm = TRUE)) else sum(num, na.rm = TRUE)
      } else if (obj$type == "species" && length(target) && !is.na(target)) {
        if (is_ts) {
          if (length(col_match)) val <- tail_mean(rowSums(num[, col_match, drop = FALSE], na.rm = TRUE))
        } else {
          rows <- tolower(trimws(as.character(df[[1]])))
          row_match <- which(rows == target)
          if (!length(row_match)) row_match <- which(grepl(target, rows, fixed = TRUE))
          if (length(row_match)) val <- sum(num[row_match[1], ], na.rm = TRUE)
          else if (length(col_match)) val <- sum(num[, col_match, drop = FALSE], na.rm = TRUE)
        }
      } else if (obj$type == "biodiversity") {
        val <- if (is_ts) tail_mean(apply(num, 1, shannon)) else shannon(rowSums(num, na.rm = TRUE))
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
    res[[objectives$name[k]]] <- 100 * (1 + a * x - b * x^2 + 0.2 * dep * (k %% 2 - 0.5)) * (1 + rnorm(1, 0, 0.01))
  }
  res
}

# Scoring ----

# Derived columns that ls_score() recomputes from scratch
ls_strip_scores <- function(df) {
  if (is.null(df)) return(NULL)
  drop <- names(df) %in% c("Composite_Score", "Pareto") | grepl("_vs_status_quo_pct$", names(df))
  df[, !drop, drop = FALSE]
}

ls_score <- function(df, objectives) {
  ok <- df$finished %in% TRUE
  df$Composite_Score <- NA_real_
  df$Pareto <- FALSE
  if (!any(ok)) return(df)
  comp <- rep(0, nrow(df))
  for (k in seq_len(nrow(objectives))) {
    obj <- objectives[k, ]
    v <- suppressWarnings(as.numeric(df[[obj$name]]))
    if (is.null(df[[obj$name]])) v <- rep(NA_real_, nrow(df))
    rng <- range(v[ok], na.rm = TRUE, finite = TRUE)
    norm <- if (all(is.finite(rng)) && rng[2] > rng[1]) (v - rng[1]) / (rng[2] - rng[1]) else rep(0, nrow(df))
    if (obj$direction == "min") norm <- 1 - norm
    norm[is.na(norm)] <- 0
    comp <- comp + norm * ls_num(obj$weight, 1)
  }
  df$Composite_Score[ok] <- comp[ok]

  # Cost-efficiency frontier: no other run scores higher for the same or lower cost
  cand <- which(ok & is.finite(df$total_cost))
  for (a in cand) {
    b <- setdiff(cand, a)
    df$Pareto[a] <- !any(df$Composite_Score[b] >= df$Composite_Score[a] & df$total_cost[b] <= df$total_cost[a] &
                         (df$Composite_Score[b] > df$Composite_Score[a] | df$total_cost[b] < df$total_cost[a]))
  }

  # Percent change of each objective relative to the no-change baseline (status quo)
  base <- which(df$kind == "baseline_none" & ok)
  for (nm in objectives$name) {
    v <- if (is.null(df[[nm]])) rep(NA_real_, nrow(df)) else suppressWarnings(as.numeric(df[[nm]]))
    b <- if (length(base)) v[base[1]] else NA_real_
    df[[paste0(nm, "_vs_status_quo_pct")]] <- if (is.finite(b) && b != 0) 100 * (v - b) / abs(b) else NA_real_
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

  S$P <- ls_build_problem(cfg, S$session_dir)
  ls_save_config(cfg, file.path(S$session_dir, "solver_config.json"))

  S$total_runs <- round(ls_num(cfg$search$iterations, 0)) + if (isTRUE(cfg$search$include_baselines)) 2 else 0
  S$i       <- 0
  S$results <- NULL
  S$cands   <- list()
  S$seen    <- character()
  S$log     <- character()
  S$results_file <- file.path(S$session_dir, "optimization_results.csv")
  S$closed  <- FALSE

  # Overwrite mode: Ecospace reads the Inputs maps, so guard them with a lock for crash recovery
  if (cfg$search$map_mode == "overwrite" && !isTRUE(cfg$search$dry_run)) {
    write_json(list(snapshot_dir = S$P$snapshot_dir, files = S$P$layers$file),
               file.path(cfg$model$inputs_dir, ls_lock_name), auto_unbox = TRUE)
    S$stconfig <- ls_write_stconfig(cfg$model$stconfig, setNames(S$P$layers$file, S$P$layers$name),
                                    file.path(S$session_dir, "STConfig_session.xml"))
  }
  ls_log(S, sprintf("Session %s | %s | strategy: %s | %d runs%s", basename(S$session_dir), basename(cfg$model$model_file),
                    cfg$strategy$type, S$total_runs, if (isTRUE(cfg$search$dry_run)) " | DRY RUN (synthetic objectives)" else ""))
  S
}

ls_log <- function(S, msg) {
  line <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)
  S$log <- c(S$log, line)
  message(line)
  cat(line, "\n", file = file.path(S$session_dir, "solver_log.txt"), append = TRUE)
}

ls_elite <- function(S) {
  if (is.null(S$results) || !any(is.finite(S$results$Composite_Score))) return(NULL)
  S$cands[[S$results$run[which.max(S$results$Composite_Score)]]]
}

ls_next_job <- function(S) {
  if (S$i >= S$total_runs) return(NULL)
  S$i <- S$i + 1
  idx <- S$i
  cfg <- S$cfg
  base <- isTRUE(cfg$search$include_baselines)
  kind <- if (base && idx == 1) "baseline_none" else if (base && idx == 2) "baseline_all" else {
    n_done <- idx - if (base) 2 else 0
    if (cfg$search$algorithm == "evolutionary" && n_done > ls_num(cfg$search$warmup, 0) && !is.null(ls_elite(S))) "evolutionary" else "random"
  }
  cand <- NULL
  for (attempt in seq_len(max(1, ls_num(cfg$search$max_retries, 50)))) {
    cand <- ls_generate_candidate(S$P, kind, if (kind == "evolutionary") ls_elite(S) else NULL)
    if (kind %in% c("baseline_none", "baseline_all")) break
    if (cand$feasible && !ls_candidate_key(cand) %in% S$seen) break
  }
  suffix <- c(baseline_none = "_Baseline_None", baseline_all = "_Baseline_All")[kind]
  run_name <- sprintf("Run_%04d%s", idx, if (is.na(suffix)) "" else suffix)
  S$seen <- c(S$seen, ls_candidate_key(cand))
  S$cands[[run_name]] <- cand
  list(idx = idx, run = run_name, kind = kind, cand = cand,
       run_dir = file.path(S$session_dir, run_name), started = Sys.time())
}

# Writes maps + command file and launches the console (or evaluates synthetically)
ls_start_job <- function(S, job) {
  P <- S$P; cfg <- S$cfg
  dir.create(job$run_dir, recursive = TRUE, showWarnings = FALSE)
  write_json(list(kind = job$kind, install = job$cand$install, decom = job$cand$decom),
             file.path(job$run_dir, "candidate.json"))
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

  if (cfg$search$map_mode == "overwrite") {
    for (nm in names(job$map_files)) file.copy(job$map_files[[nm]], P$layers$file[P$layers$name == nm], overwrite = TRUE)
    stconfig <- S$stconfig
  } else {
    stconfig <- ls_write_stconfig(cfg$model$stconfig, job$map_files, file.path(job$run_dir, "STConfig_run.xml"))
  }
  cmd <- ls_write_cmd(P, job$run_dir, stconfig)

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
    ls_evaluate_objectives(job$run_dir, cfg$objectives, cfg$search$eval_last_n)
  }
  s <- job$layers$structure
  total_cost <- cand$install_cost + cand$decom_cost
  roi_obj <- obj[[cfg$search$roi_objective %||% ""]]
  row <- data.frame(
    run = job$run, iteration = job$idx, kind = job$kind,
    n_installed = length(cand$install), n_decommissioned = length(cand$decom),
    n_structures = sum(s > 0, na.rm = TRUE), structure_coverage = round(sum(s, na.rm = TRUE), 4),
    install_cost = round(cand$install_cost, 2), decom_cost = round(cand$decom_cost, 2),
    total_cost = round(total_cost, 2),
    ROI_per_dollar = if (!is.null(roi_obj) && !is.na(roi_obj) && total_cost > 0) roi_obj / total_cost else NA_real_,
    finished = finished, exit_status = job$status, dry_run = dry,
    runtime_sec = round(as.numeric(difftime(Sys.time(), job$started, units = "secs")), 1),
    map_dir = file.path(job$run_dir, "Maps"), timestamp = ls_timestamp(),
    stringsAsFactors = FALSE
  )
  row <- cbind(row, as.data.frame(obj, check.names = FALSE))
  write.csv(row, file.path(job$run_dir, "run_summary.csv"), row.names = FALSE)

  S$results <- ls_score(rbind(ls_strip_scores(S$results), row), cfg$objectives)
  write.csv(S$results, S$results_file, row.names = FALSE)

  sc <- S$results$Composite_Score[nrow(S$results)]
  ls_log(S, sprintf("%s (%s) | +%d / -%d structures | %s | %s | score %s",
                    job$run, job$kind, row$n_installed, row$n_decommissioned, ls_usd(total_cost),
                    if (finished) "finished" else sprintf("FAILED (exit %s)", job$status),
                    if (is.na(sc)) "NA" else sprintf("%.3f", sc)))
  invisible(row)
}

# Kill any running console, put pristine maps back and remove the lock
ls_close_session <- function(S, job = NULL) {
  if (isTRUE(S$closed)) return(invisible(NULL))
  if (!is.null(job$proc) && job$proc$is_alive()) job$proc$kill_tree()
  cfg <- S$cfg
  if (cfg$search$map_mode == "overwrite" && !isTRUE(cfg$search$dry_run)) {
    for (f in S$P$layers$file) file.copy(file.path(S$P$snapshot_dir, basename(f)), f, overwrite = TRUE)
    unlink(file.path(cfg$model$inputs_dir, ls_lock_name))
    ls_log(S, "Restored pristine maps to the Inputs folder.")
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

# Layout map: 0 land, 1 water, 2 eligible install, 3 eligible decom, 4 existing kept,
# 5 new installation, 6 decommissioned
ls_layout_codes <- function(P, cand = NULL, show_eligible = TRUE) {
  z <- ifelse(P$water, 1, 0)
  z[P$existing > 0 & P$water] <- 4
  if (show_eligible) {
    z[P$install_valid]  <- 2
    z[P$decom_eligible] <- 3
  }
  if (!is.null(cand)) {
    z[cand$install] <- 5
    z[cand$decom]   <- 6
  }
  z
}

ls_layout_colors <- c("#d9d9d9", "#dbe9f6", "#b8e0b0", "#f6c7c7", "#4d4d4d", "#f28e1c", "#d62728")
ls_layout_labels <- c("Land", "Water", "Eligible: install", "Eligible: decommission",
                      "Existing structure", "New structure", "Decommissioned")

ls_plot_layout <- function(P, cand = NULL, main = "", show_eligible = TRUE) {
  z <- ls_layout_codes(P, cand, show_eligible)
  m <- matrix(z, nrow = P$nr, ncol = P$nc, byrow = TRUE)
  e <- as.vector(ext(P$template))
  xs <- seq(e[1], e[2], length.out = P$nc + 1)
  ys <- seq(e[3], e[4], length.out = P$nr + 1)
  op <- par(mar = c(3, 3, 2, 1), mgp = c(1.8, 0.5, 0))
  on.exit(par(op))
  image(xs, ys, t(m[P$nr:1, , drop = FALSE]), col = ls_layout_colors, breaks = seq(-0.5, 6.5, 1),
        xlab = "Longitude", ylab = "Latitude", main = main, useRaster = TRUE,
        asp = 1 / cos(mean(e[3:4]) * pi / 180))
  present <- sort(unique(z))
  legend("bottomright", legend = ls_layout_labels[present + 1], fill = ls_layout_colors[present + 1],
         cex = 0.75, bg = "white", box.col = "grey70")
}
