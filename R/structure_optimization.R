#####################################################
### Artificial Structure Optimization Loop Script ###
#####################################################

# Load libraries and dependencies ----
library(terra)
library(xml2)

# Directory Setup ----
indir     <- "../Inputs/NGoM"
parent_out <- "../Outputs/NGoM"

# Create a session-specific directory (datetime)
session_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
session_dir       <- file.path(parent_out, session_timestamp)
if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)

# Configuration Files ----
stconfig_base <- file.path(indir, "NGoMSTConfig.xml")
cmd_base_path <- file.path(indir, "NGoM_CommandFile_template.txt")
file_console  <- "C:/Program Files/EcoSpace Console 1.4.3.1 64-bit/EwEClientConsole.exe"

# Parameters ----
iterations           <- 1000
mode                 <- 1 # 1: Random searching
imp_year             <- 2019
budget_limit         <- 500000 # Example budget in USD

# Load Spatial Constraints ----

# Depth mask/Basemap
basemap <- rast(file.path(indir, "basemap.asc"))

# Current structure map (Existing reefs)
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
  # Default: Allow everything where depth is positive
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

# Model Execution Function ----

fn.run_model <- function(cmdfile) {
  run_dir <- dirname(cmdfile)
  target_file <- file.path(run_dir, "Ecospace_Annual_Average_Biomass.csv")
  
  # Normalize paths for Windows
  console_path <- normalizePath(file_console, winslash = "\\", mustWork = TRUE)
  cmd_path     <- normalizePath(cmdfile, winslash = "\\", mustWork = TRUE)
  
  # Execute directly via R system call
  system(paste0('"', console_path, '" "', cmd_path, '"'), wait = FALSE, invisible = FALSE)
  
  start_time <- Sys.time()
  model_finished <- FALSE
  
  # Monitoring loop (10 minute timeout)
  while(difftime(Sys.time(), start_time, units="mins") < 10) {
    Sys.sleep(5)
    if(file.exists(target_file)) {
      if(file.info(target_file)$size > 100) {
        Sys.sleep(2) # Disk write buffer
        model_finished <- TRUE
        break
      }
    }
  }
  
  # Cleanup: Ensure the console process is closed to release file locks
  try(system("taskkill /F /IM EwEClientConsole.exe", show.output.on.console = FALSE), silent = TRUE)
  
  return(model_finished)
}

# Optimization Loop Helper ----

fn.create_and_run <- function(idx) {
  
  # 1. Create Run Directory: outdir/datetime/Run_XXXX
  run_name  <- sprintf("Run_%04d", idx)
  run_dir   <- file.path(session_dir, run_name)
  map_dir   <- file.path(run_dir, "Maps")
  dir.create(map_dir, recursive = TRUE)
  
  # 2. Logic for adding structures
  current_struct <- structures
  annual_budget_spent <- 0
  
  # Identify valid indices
  valid_mask <- (exclusions > 0) & (current_struct == 0)
  valid_indices <- which(values(valid_mask, mat=FALSE) == 1)
  
  # Debug info on the first iteration
  if(idx == 1) {
    message("--- Data Summary ---")
    message(paste("Exclusions value range:", min(values(exclusions), na.rm=T), "to", max(values(exclusions), na.rm=T)))
    message(paste("Structures value range:", min(values(structures), na.rm=T), "to", max(values(structures), na.rm=T)))
    message(paste("Total valid cells available for deployment:", length(valid_indices)))
    message(paste("Output Session Directory:", session_dir))
    message("--------------------")
  }
  
  if(length(valid_indices) == 0) {
    warning("No valid indices found for structure implementation. Check exclusion values.")
    return(FALSE)
  }
  
  # Get all costs once to speed up lookup
  all_costs <- values(cost_map, mat=FALSE)
  
  while (annual_budget_spent < budget_limit && length(valid_indices) > 0) {
    target_idx <- sample(valid_indices, 1)
    
    # Update current_struct value
    current_struct[target_idx] <- 1
    
    # Extract cost
    cell_cost <- as.numeric(all_costs[target_idx])
    if(is.na(cell_cost) || cell_cost < 0) cell_cost <- 0
    
    annual_budget_spent <- annual_budget_spent + cell_cost
    
    # Remove index from candidates
    valid_indices <- valid_indices[valid_indices != target_idx]
  }
  
  # 3. Save Habitat Map (Raster) in run folder
  map_name <- paste0("artificial_reef_", imp_year, ".asc")
  writeRaster(current_struct, file.path(map_dir, map_name),
              filetype = "AAIGrid", overwrite = TRUE, NAflag = -9999)
  
  # 4. Prepare STConfig.xml in run folder
  if (file.exists(stconfig_base)) {
    stconfig <- read_xml(stconfig_base)
    source_node <- xml_find_first(stconfig, "//Configuration[Name='artificial reef']/Source")
    files_node  <- xml_find_first(stconfig, "//Configuration[Name='artificial reef']/Files")
    
    xml_remove(xml_children(files_node)) 
    xml_add_child(files_node, "File", map_name, Date = paste0(imp_year, "-01-01"))
    xml_text(source_node) <- normalizePath(map_dir, winslash = "\\")
    
    write_xml(stconfig, file.path(run_dir, "STConfig.xml"))
  }
  
  # 5. Prepare Command File in run folder
  if (file.exists(cmd_base_path)) {
    cmd_lines <- readLines(cmd_base_path)
    # Update output directory to this specific run's folder
    cmd_lines <- gsub("<ECOSPACE_OUTPUT_DIR>.*",
                      paste0("<ECOSPACE_OUTPUT_DIR>, ", normalizePath(run_dir, winslash = "\\"), ", System.String, Updated"),
                      cmd_lines)
    # Update STConfig path to the local one we just created
    cmd_lines <- gsub("<ECOSPACE_ST_CONFIG_FILE>.*",
                      paste0("<ECOSPACE_ST_CONFIG_FILE>, ", normalizePath(file.path(run_dir, "STConfig.xml"), winslash = "\\"), ", System.String, Updated"),
                      cmd_lines)
    
    cmd_file <- file.path(run_dir, "cmd.txt")
    writeLines(cmd_lines, cmd_file)
    
    # 6. Execute Model
    message(paste("Starting model for", run_name))
    success <- fn.run_model(cmd_file)
    return(success)
  }
  
  return(FALSE)
}

# Execute ----

if (mode == 1) {
  for (i in 1:iterations) {
    status <- fn.create_and_run(i)
    if(status) {
      message(paste("Iteration", i, "completed successfully."))
    } else {
      warning(paste("Iteration", i, "failed or timed out."))
    }
  }
}