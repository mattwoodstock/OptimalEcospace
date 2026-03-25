#####################################################
### Artiifical Structure Optimization Loop Script ###
###                                               ###
### Purpose: Optimize artificial structure        ###
###  implementation based on management needs     ###
#####################################################

# Citation: work in progress
# ----------------------------------------------------#

# Load libraries and dependencies ----
library(terra)
library(xml2)
indir <- "../Inputs/NGoM"
outdir <- "../Outputs/NGoM"
stconfig_base <- file.path(indir,"STConfig.xml") #* Need to create *#
stconfig_habitat_pointer <- NA #Add in full pointer that leads to habitat changes in STConfig file. Requires initial "base" run to load in a habitat map at some point to know file path.
cmd_base <- file.path(indir,"cmd_base.txt")
iterations <- 1000
mode = 1 #For dumb (random) searching. In the future, we'll want two modes iterative and paralleled
start_year = 2009 #Start year of the model
imp_year = 2019 #Implementation year of structure
implementation_years <- 1

# Load manager preferences ----

#****
#How should we actually do this? I think it should be implemented as a list, but do we have a standard CSV form that gets loaded here and added to the list?
#****

## Create initial constraints for optimization ----
budget <- NA #Insert budget here ($USD)

# Depth mask of the model
basemap <- rast(file.path(indir,"basemap.asc"))

#Current structure map
structures <- rast(file.path(indir,"structures.asc"))

#Locations where there could not be any structure implementation.
## Some considerations: minimum depth (m), other spatial uses (shipping, military)
if (file.exists("exclusions.asc")){
  exclusions <- raster("exculsions.asc") 
} else {
  exlcusions <- basemap
}

# Cost map
if (file.exists(file.path(indir,"cost_map.asc"))){
  cost_map <- rast(file.path(indir,"cost_map.asc"))
} else {
  cost_map <- basemap
  cost_map <- cost_map[cost_map > 0] <- 1000 #Adding a random value here.
}
cost_map <- rast(file.path(indir,"cost_map.asc"))

# Habitat map functions ----

## Check maps to assure they are the same resolution
fn.check_maps <- function(base,struct,excl){
  if (ext(base) != ext(struct)){
    error("Basemap and Structure map do not match")
  }
  if (ext(base) != ext(excl)){
    error("Basemap and Structure map do not match")
  }
}

## Create habitat maps
fn.create_structure <- function(outdir, base,struct,exclude,cost,start_year,imp_year,implementation_years,budget = 1000){
  # Create custom, random directory
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  raw_dir <- file.path(outdir, paste0("Run_", timestamp))
  run_dir <<- normalizePath(raw_dir, winslash = "\\", mustWork = FALSE)
  dir.create(run_dir, recursive = TRUE)
  
  # Develop maps
  map_dir <- file.path(run_dir,"Maps")
  stconfig_file <- read_xml(stconfig_base)
  source_node <- xml_find_first(stconfig_file, "//Configuration[Name='artificial reef']/Source")
  files_parent_node <- xml_find_first(stconfig_file, "//Configuration[Name='artificial reef']/Files")
  file_nodes <- xml_find_all(files_parent_node, "File")
  xml_remove(old_files)
  files <- c()
  dates <- c()
  for (i in 1:implementation_years){
    annual_budget_spent <- 0
    
    while (annual_budget_spent < budget){
      rand1 = runif(1,min=1,max=nrow(base))
      rand2 = runif(1,min=1,max=ncol(base))
      
      struct_val = struct[rand1,rand2]
      exclude_val = exclude[rand1,rand2]
      while (struct_val == 1 | exclude_val == 0){
        rand1 = runif(1,min=1,max=nrow(base))
        rand2 = runif(1,min=1,max=ncol(base))
        
        struct_val = struct[rand1,rand2]
        exclude_val = exclude[rand1,rand2]
      }
      struct[rand1,rand2] = 1 #Want to complicate to have a size? Also will need to adjust other habitats and add at the same time.
      annual_budget_spent = annual_budget_spent + cost[rand1,rand2]
      
      files <- c(files,paste0("artificial reef_",start_year+(i-1),"01-01.asc"))
      dates <- c(dates,paste0(start_year+(i-1),"01-01"))
      writeRaster(struct,file.path(mapdir,files[length(files)]))
    }
  }
  # Add the new file nodes to the <Files> parent node
  for (i in seq_along(files)) {
    xml_add_child(files_parent_node, 
                  "File", 
                  files[i], 
                  Date = dates[i])
  }
  
  # Create and output STConfig File
  xml_text(source_node) <- map_dir
  write_xml(stconfig_file, file.path(run_dir, "STConfig.xml"))
  
  # Adjust and output command file
  
  write.table(new_cmd, file.path(run_dir, "cmd.txt"))
}

## Execute optimization ----
fn.check_maps(basemap,structures,exclusions)

if (mode == 1){ #Random, rather than convergent
  for (i in 1:iterations){
    
  }
} else if (mode == 2) {

} else {
  error("Improper mode. Assign either 1 (random) or 2 (converging)")
}

