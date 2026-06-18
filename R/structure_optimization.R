#####################################################
### Artificial Structure Optimization Loop Script ###
#####################################################
#
# Workflow per iteration:
#   1. Build an artificial-reef habitat map (0/1) under a budget constraint
#   2. Proportionally reduce coverage of existing natural habitats in reef cells
#   3. ARCHIVE the maps in the Run folder for your records
#   4. OVERWRITE the base maps directly in the Inputs folder
#   5. Point a per-run command file at a per-run output dir (XML is handled natively)
#   6. Call the Ecospace console (EwEClientConsole.exe) and wait
#   7. Read the run's objective from the LAST YEAR of the output CSV and log it
#

# Load libraries and dependencies ----
library(terra)
library(xml2)

# Directory Setup ----
# Ensure we are using consistent absolute paths to avoid naming issues
indir      <- normalizePath("../Inputs/NGoM", winslash = "/", mustWork = TRUE)
parent_out <- normalizePath("../Outputs/NGoM", winslash = "/", mustWork = FALSE)

if (!dir.exists(parent_out)) dir.create(parent_out, recursive = TRUE)

# Create a session-specific directory (datetime)
session_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
session_dir       <- file.path(parent_out, session_timestamp)
if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)

# Configuration Files ----
cmd_base_path <- file.path(indir, "NGoM_CommandFile_template.txt")
file_console  <- "C:/Program Files/EcoSpace Console 1.4.3.1 64-bit/EwEClientConsole.exe"

# Parameters ----
iterations           <- 1000   # Number of random searches (Note: script adds 2 baseline runs to this total)
mode                 <- 1      # 1: Random searching
budget_limit         <- 500000 # Example budget in USD

# How much of the cell does 1 unit of artificial reef replace? (1.0 = 100% replacement)
reef_coverage_fraction <- 1.0 

# Define the names of natural habitats EXACTLY as they appear in the Inputs folder
natural_habitats <- list(
  "gravel" = "gravel.asc",
  "rock"   = "rock.asc",
  "mud"    = "mud.asc",
  "sand"   = "sand.asc"
)

# Prevents one hung console process from stalling the whole loop. 0 = no timeout.
run_timeout_sec      <- 1800

# Define the multiple objectives you want to extract from the simulation.
objectives <- list(
  list(name = "RS_Biomass",       file_pattern = "Annual_Average_Biomass", target = "red snapper adult", type = "species",      direction = "max", weight = 1.0),
  list(name = "Tot_Biomass",      file_pattern = "Annual_Average_Biomass", target = NA,                  type = "sum",          direction = "max", weight = 0.5),
  list(name = "RS_Catch",         file_pattern = "Annual_Average_Catch",   target = "red snapper adult", type = "species",      direction = "max", weight = 2.0),
  list(name = "Tot_Catch",        file_pattern = "Annual_Average_Catch",   target = NA,                  type = "sum",          direction = "max", weight = 0.5),
  list(name = "Biodiversity",     file_pattern = "Annual_Average_Biomass", target = NA,                  type = "biodiversity", direction = "max", weight = 1.0),
  list(name = "Lionfish_Biomass", file_pattern = "Annual_Average_Biomass", target = "lionfish",          type = "species",      direction = "min", weight = 1.0)
)

# master results table for the whole session (one row per run).
results_file <- file.path(session_dir, "optimization_results.csv")

# Small helper: Appends rows safely to the master file
fn.append_result <- function(row_df, file_path) {
  tryCatch({
    write.table(row_df, file = file_path, sep = ",", 
                row.names = FALSE, col.names = !file.exists(file_path), 
                append = file.exists(file_path))
  }, error = function(e) NULL)
}

# Small helper: AAIGrid writes sidecar files (.prj, .asc.aux.xml).
fn.cleanup_aux <- function(asc_path) {
  stem <- sub("\\.asc$", "", asc_path, ignore.case = TRUE)
  for (ext in c(".prj", ".asc.aux.xml", ".aux", ".asc.xml")) {
    f <- paste0(stem, ext)
    if (file.exists(f)) try(file.remove(f), silent = TRUE)
  }
}

# SAFEGUARD: Backup the pristine natural habitats before we start overwriting them!
backup_dir <- file.path(indir, "Backup_Base_Maps")
if (!dir.exists(backup_dir)) dir.create(backup_dir)

message("Backing up base maps to prevent data loss...")
for (hab in names(natural_habitats)) {
  orig_file <- file.path(indir, natural_habitats[[hab]])
  if (file.exists(orig_file)) {
    file.copy(orig_file, file.path(backup_dir, natural_habitats[[hab]]), overwrite = TRUE)
  }
}
if (file.exists(file.path(indir, "structures_all.asc"))) {
  file.copy(file.path(indir, "structures_all.asc"), file.path(backup_dir, "structures_all.asc"), overwrite = TRUE)
}

# Backup and repair STConfig.xml to guarantee it points to the Inputs folder
if (file.exists(stconfig_base)) {
  file.copy(stconfig_base, file.path(backup_dir, basename(stconfig_base)), overwrite = TRUE)
  
  # Force paths to Inputs directory (fixes issue where XML was accidentally saved pointing to Outputs)
  doc <- read_xml(stconfig_base)
  xml_ns_strip(doc)
  all_habs <- c(setNames("structures_all.asc", reef_habitat_name), natural_habitats)
  
  for (hab_name in names(all_habs)) {
    xpath <- sprintf("//Configuration[translate(Name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')='%s']", tolower(hab_name))
    node <- xml_find_first(doc, xpath)
    if (!is.na(node)) {
      file_node <- xml_find_first(node, "File")
      if (!is.na(file_node)) {
        xml_text(file_node) <- normalizePath(file.path(indir, all_habs[[hab_name]]), winslash = "\\", mustWork = FALSE)
      }
    }
  }
  write_xml(doc, stconfig_base)
}

# Load Spatial Constraints ----

# Depth mask/Basemap
basemap <- rast(file.path(indir, "basemap.asc"))

# Current structure map (Existing reefs - fallback if structures.asc exists)
if (file.exists(file.path(indir, "structures.asc"))) {
  structures <- rast(file.path(indir, "structures.asc"))
} else {
  structures <- basemap
  structures[] <- 0
}

# Exclusion map (Area where structures are allowed)
if (file.exists(file.path(indir, "exclusions.asc"))) {
  exclusions <- rast(file.path(indir, "exclusions.asc"))
} else {
  exclusions <- basemap > 0
}

# Cost map (default = $1000 per meter of water)
if (file.exists(file.path(indir, "cost_map.asc"))) {
  cost_map <- rast(file.path(indir, "cost_map.asc"))
} else {
  cost_map <- basemap * 1000
}

# Alignment check to prevent "non-conformable" errors
if (!compareGeom(basemap, structures, stopOnError = FALSE) ||
    !compareGeom(basemap, exclusions, stopOnError = FALSE) ||
    !compareGeom(basemap, cost_map, stopOnError = FALSE)) {
  
  message("Aligning spatial layers to basemap...")
  structures <- resample(structures, basemap, method = "near")
  exclusions <- resample(exclusions, basemap, method = "near")
  cost_map   <- resample(cost_map, basemap, method = "bilinear")
}

# Pre-extract values once (vectors are far faster than per-cell raster indexing)
base_vals <- values(basemap,    mat = FALSE)
excl_vals <- values(exclusions, mat = FALSE)
cost_vals <- values(cost_map,   mat = FALSE)
str_vals0 <- values(structures, mat = FALSE)
str_vals0[is.na(str_vals0)] <- 0          
na_mask   <- is.na(base_vals)             
cost_vals[is.na(cost_vals) | cost_vals < 0] <- 0

# Load background natural habitats into a list of vectors FROM THE BACKUP (Pristine state)
base_hab_vals <- list()
for (hab in names(natural_habitats)) {
  fpath <- file.path(backup_dir, natural_habitats[[hab]])
  if (file.exists(fpath)) {
    r <- rast(fpath)
    if (!compareGeom(basemap, r, stopOnError = FALSE)) {
      r <- resample(r, basemap, method = "bilinear")
    }
    base_hab_vals[[hab]] <- values(r, mat = FALSE)
  } else {
    warning(sprintf("Natural habitat file '%s' not found in backup. Will be skipped.", fpath))
  }
}

# Cells where a NEW reef may be placed: allowed by exclusions, not already reef, on water
base_valid_mask <- (!is.na(excl_vals) & excl_vals > 0) & (str_vals0 == 0) & (!na_mask)
base_valid_idx  <- which(base_valid_mask)

message("--- Data Summary ---")
message(sprintf("Grid cells: %d (water-valid for deployment: %d)", length(base_vals), length(base_valid_idx)))
message(sprintf("Existing reef cells carried forward: %d", sum(str_vals0 > 0, na.rm = TRUE)))
message(sprintf("Output Session Directory: %s", session_dir))
message("--------------------")

if (length(base_valid_idx) == 0) {
  stop("No valid cells available for deployment. Check exclusions / basemap values.")
}

# Model Execution Function ----

fn.run_model <- function(cmdfile, run_dir) {
  # Normalize paths for Windows execution
  console_path <- normalizePath(file_console, winslash = "\\", mustWork = TRUE)
  cmd_path     <- normalizePath(cmdfile,      winslash = "\\", mustWork = TRUE)
  
  # Run and capture exit status
  status <- tryCatch(
    system2(console_path, args = shQuote(cmd_path),
            wait    = TRUE,
            timeout = run_timeout_sec,
            stdout  = file.path(run_dir, "console_stdout.log"),
            stderr  = file.path(run_dir, "console_stderr.log")),
    error   = function(e) { message("  console error: ", conditionMessage(e)); 999L },
    warning = function(w) { message("  console warning: ", conditionMessage(w)); -1L }
  )
  
  try(system("taskkill /F /IM EwEClientConsole.exe /FI \"STATUS eq RUNNING\"", show.output.on.console = FALSE), silent = TRUE)
  
  hits <- list.files(run_dir, pattern = "Ecospace_.*\\.csv$", recursive = TRUE)
  finished <- length(hits) > 0
  list(finished = finished, status = status)
}

# Objective Readers ---- 
fn.read_ewe_csv <- function(run_dir, file_pattern) {
  hits <- list.files(run_dir, pattern = paste0(file_pattern, ".*\\.csv$"), recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(hits) == 0) return(NULL)
  
  csv_path <- hits[1]
  lines <- tryCatch(readLines(csv_path), error = function(e) NULL)
  if (is.null(lines) || length(lines) == 0) return(NULL)
  
  stripped_lines <- gsub('".*?"', '', lines)
  comma_counts <- sapply(gregexpr(",", stripped_lines), function(x) if(x[1] == -1) 0 else length(x))
  max_commas <- max(comma_counts)
  if (max_commas == 0) return(NULL)
  
  header_idx <- which(comma_counts == max_commas)[1]
  skip_amount <- header_idx - 1
  
  df <- tryCatch(
    read.csv(csv_path, skip = skip_amount, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  return(df)
}

fn.evaluate_objectives <- function(run_dir, objectives_list) {
  results <- list()
  cached_csvs <- list() 
  
  for (obj in objectives_list) {
    val <- NA_real_
    
    if (is.null(cached_csvs[[obj$file_pattern]])) {
      cached_csvs[[obj$file_pattern]] <- fn.read_ewe_csv(run_dir, obj$file_pattern)
    }
    df <- cached_csvs[[obj$file_pattern]]
    
    if (!is.null(df) && nrow(df) > 0 && ncol(df) > 1) {
      first_col_name <- tolower(trimws(colnames(df)[1]))
      is_time_series <- any(grepl("time|year|step", first_col_name))
      
      data_cols <- df[, -1, drop = FALSE]
      num_mat <- suppressWarnings(matrix(as.numeric(as.character(unlist(data_cols))), nrow = nrow(data_cols), ncol = ncol(data_cols)))
      
      if (obj$type == "sum") {
        if (is_time_series) {
          yearly_totals <- rowSums(num_mat, na.rm = TRUE)
          val <- yearly_totals[length(yearly_totals)]
        } else {
          val <- sum(num_mat, na.rm = TRUE)
        }
      } else if (obj$type == "species" && !is.na(obj$target)) {
        target_clean <- tolower(trimws(obj$target))
        cols_clean   <- tolower(trimws(colnames(df)))
        first_col_clean <- tolower(trimws(as.character(df[[1]])))
        
        col_match <- which(cols_clean == target_clean)
        if (length(col_match) == 0) col_match <- which(grepl(target_clean, cols_clean, fixed = TRUE))
        
        row_match <- which(first_col_clean == target_clean)
        if (length(row_match) == 0) row_match <- which(grepl(target_clean, first_col_clean, fixed = TRUE))
        
        if (is_time_series) {
          valid_cols <- col_match[col_match > 1] - 1 
          if (length(valid_cols) > 0) {
            yearly_sums <- rowSums(num_mat[, valid_cols, drop = FALSE], na.rm = TRUE)
            val <- yearly_sums[length(yearly_sums)]
          }
        } else {
          if (length(row_match) > 0) {
            row_idx <- row_match[1]
            val <- sum(num_mat[row_idx, ], na.rm = TRUE)
          } else if (length(col_match) > 0) {
            valid_cols <- col_match[col_match > 1] - 1
            if (length(valid_cols) > 0) {
              val <- sum(num_mat[, valid_cols, drop = FALSE], na.rm = TRUE)
            }
          }
        }
      } else if (obj$type == "biodiversity") {
        if (is_time_series) {
          shannon_idx <- apply(num_mat, 1, function(row) {
            p <- row[row > 0] / sum(row[row > 0], na.rm = TRUE)
            if (length(p) == 0) return(0)
            -sum(p * log(p), na.rm = TRUE)
          })
          val <- shannon_idx[length(shannon_idx)]
        } else {
          group_totals <- rowSums(num_mat, na.rm = TRUE)
          p <- group_totals[group_totals > 0] / sum(group_totals[group_totals > 0], na.rm = TRUE)
          if(length(p) > 0) val <- -sum(p * log(p), na.rm = TRUE)
        }
      }
    }
    if (is.na(val) || is.nan(val) || is.infinite(val)) val <- NA_real_
    results[[obj$name]] <- val
  }
  return(results)
}

# Optimization Loop Helper ----

fn.create_and_run <- function(idx) {
  
  if (idx == 1) {
    run_name <- "Run_0001_Baseline_None"
  } else if (idx == 2) {
    run_name <- "Run_0002_Baseline_All"
  } else {
    run_name <- sprintf("Run_%04d", idx)
  }
  
  run_dir   <- file.path(session_dir, run_name)
  map_dir   <- file.path(run_dir, "Maps")
  dir.create(map_dir, recursive = TRUE)
  
  default_obj_results <- setNames(lapply(objectives, function(x) NA_real_), sapply(objectives, `[[`, "name"))
  
  exit_early <- function(status_code = -1) {
    df_row <- data.frame(
      run          = run_name,
      iteration    = idx,
      n_reef_new   = if (exists("chosen")) length(chosen) else 0,
      budget_spent = if (exists("spent")) round(spent, 2) else 0,
      ROI_Catch_Per_Dollar = 0,
      finished     = FALSE,
      exit_status  = status_code,
      map_path     = if (exists("map_path_archive")) map_path_archive else "",
      timestamp    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    df_row <- cbind(df_row, as.data.frame(default_obj_results))
    write.csv(df_row, file.path(run_dir, "run_summary.csv"), row.names = FALSE)
    fn.append_result(df_row, results_file)
    return(df_row)
  }
  
  if (idx == 1) {
    chosen <- integer(0)
    spent  <- 0
  } else if (idx == 2) {
    chosen <- base_valid_idx
    spent  <- sum(cost_vals[chosen], na.rm = TRUE)
  } else {
    shuffled  <- base_valid_idx[sample.int(length(base_valid_idx))]
    cum_cost  <- cumsum(cost_vals[shuffled])
    keep      <- cum_cost <= budget_limit
    chosen    <- shuffled[keep]
    spent     <- if (length(chosen)) cum_cost[max(which(keep))] else 0
  }
  
  struct_vals <- str_vals0
  struct_vals[chosen]  <- reef_coverage_fraction
  struct_vals[na_mask] <- NA
  
  reef <- rast(file.path(indir, "basemap.asc"))
  values(reef) <- struct_vals
  
  # 1. Archive the map in the Run folder for your records
  map_path_archive <- file.path(map_dir, "structures_all.asc")
  writeRaster(reef, map_path_archive, filetype = "AAIGrid", overwrite = TRUE, NAflag = -9999)
  
  # 2. Forcefully OVERWRITE the active base map in the Inputs folder
  map_path_active <- file.path(indir, "structures_all.asc")
  writeRaster(reef, map_path_active, filetype = "AAIGrid", overwrite = TRUE, NAflag = -9999)
  fn.cleanup_aux(map_path_active) 
  
  # 3. Proportionally reduce, archive, and forcefully overwrite natural habitats
  for (hab_name in names(base_hab_vals)) {
    hab_vals <- base_hab_vals[[hab_name]]
    
    if (length(chosen) > 0) {
      hab_vals[chosen] <- pmax(0, hab_vals[chosen] - reef_coverage_fraction)
    }
    
    hab_rast <- rast(file.path(indir, "basemap.asc"))
    values(hab_rast) <- hab_vals
    
    # Archive
    hab_archive <- file.path(map_dir, natural_habitats[[hab_name]])
    writeRaster(hab_rast, hab_archive, filetype = "AAIGrid", overwrite = TRUE, NAflag = -9999)
    
    # Active Overwrite
    hab_active <- file.path(indir, natural_habitats[[hab_name]])
    writeRaster(hab_rast, hab_active, filetype = "AAIGrid", overwrite = TRUE, NAflag = -9999)
    fn.cleanup_aux(hab_active) 
  }
  
  # 4. Prepare Command File (We NO LONGER need to hack the STConfig file!)
  if (!file.exists(cmd_base_path)) {
    warning("Command file template not found: ", cmd_base_path)
    return(exit_early())
  }
  
  cmd_lines <- readLines(cmd_base_path)
  out_dir_win <- normalizePath(run_dir, winslash = "\\", mustWork = TRUE)
  
  set_param <- function(lines, token, value) {
    hit <- grepl(paste0("^\\s*", token), lines)
    new <- paste0(token, ", ", value, ", System.String, Updated")
    if (any(hit)) lines[hit] <- new else lines <- c(lines, new)
    lines
  }
  
  # Just override the Output directory. Ecospace will naturally read the 
  # physically overwritten ASCII files from the Inputs directory.
  cmd_lines <- set_param(cmd_lines, "<ECOSPACE_OUTPUT_DIR>", out_dir_win)
  
  cmd_file <- file.path(run_dir, "cmd.txt")
  writeLines(cmd_lines, cmd_file)
  
  # 5. Execute Model
  res <- fn.run_model(cmd_file, run_dir)
  
  # 6. Evaluate multiple objectives 
  if (res$finished) {
    obj_results <- fn.evaluate_objectives(run_dir, objectives)
  } else {
    obj_results <- default_obj_results
  }
  
  roi_val <- if (spent > 0 && !is.na(obj_results[["RS_Catch"]])) {
    obj_results[["RS_Catch"]] / spent
  } else {
    0
  }
  
  df_row <- data.frame(
    run          = run_name,
    iteration    = idx,
    n_reef_new   = length(chosen),
    budget_spent = round(spent, 2),
    ROI_Catch_Per_Dollar = roi_val,
    finished     = res$finished,
    exit_status  = res$status,
    map_path     = map_path_archive, # Log the archive path so you can retrieve the map
    timestamp    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  
  df_row <- cbind(df_row, as.data.frame(obj_results))
  
  write.csv(df_row, file.path(run_dir, "run_summary.csv"), row.names = FALSE)
  fn.append_result(df_row, results_file)
  
  return(df_row)
}

# Execute Optimization Loop ----

if (mode == 1) {
  total_runs <- iterations + 2 
  message(sprintf("Starting SEQUENTIAL optimization (Total runs: %d)...", total_runs))
  
  # Execute iterations sequentially using a standard for-loop
  results_list <- list()
  for (i in 1:total_runs) {
    message(sprintf("  -> Running iteration %d of %d...", i, total_runs))
    results_list[[i]] <- fn.create_and_run(i)
  }
  
  results_df <- do.call(rbind, results_list)
  
  # Normalize objectives and compute a Weighted Composite Score
  composite_score <- rep(0, nrow(results_df))
  
  for (obj in objectives) {
    vals <- as.numeric(results_df[[obj$name]])
    vals[is.na(vals)] <- 0 
    
    max_v <- max(vals, na.rm = TRUE)
    min_v <- min(vals, na.rm = TRUE)
    
    if (max_v > min_v) {
      norm_vals <- (vals - min_v) / (max_v - min_v)
    } else {
      norm_vals <- rep(0, length(vals)) 
    }
    
    if (obj$direction == "min") {
      norm_vals <- 1 - norm_vals
    }
    
    composite_score <- composite_score + (norm_vals * obj$weight)
  }
  
  results_df$Composite_Score <- composite_score
  write.csv(results_df, results_file, row.names = FALSE)
  
  # Clean up / Restore the backed-up pristine maps and XML
  message("Restoring original pristine maps and STConfig.xml to the Inputs folder...")
  for (f in list.files(backup_dir, full.names = TRUE)) {
    file.copy(f, file.path(indir, basename(f)), overwrite = TRUE)
  }
  
  best_idx <- which.max(results_df$Composite_Score)
  message("====================================")
  if (length(best_idx) > 0 && !is.na(best_idx)) {
    best_run <- results_df$run[best_idx]
    best_score <- results_df$Composite_Score[best_idx]
    roi_best <- results_df$ROI_Catch_Per_Dollar[best_idx]
    
    message(sprintf("Session complete. Best run: %s", best_run))
    message(sprintf("  -> Composite Score: %.2f", best_score))
    message(sprintf("  -> ROI (Catch/$):   %.6f", roi_best))
  } else {
    message("Session complete, but no valid objectives were returned.")
  }
  message(sprintf("Full results: %s", results_file))
  message("====================================")
}