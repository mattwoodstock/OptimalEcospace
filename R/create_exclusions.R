#####################################################
### Create exclusion map                          ###
###                                               ###
### Purpose: Create exclusion map for             ###
###  optimization                                 ###
#####################################################

# ----------------------------------------------------#

# Load libraries and dependencies ----
library(terra)
indir <- "../Inputs/NGoM"
basemap <- rast(file.path(indir,"basemap.asc"))

exclusion <- basemap #Set initial exclusion map equal to basemap
# Depth-based exclusion ----
min_depth <- 10 # Minimum depth in meters
max_depth <- 100 #Maximum depth in meters

fn.exclude_depth <- function(base,exclude,min=0,max = 1e20){
  if (!is.na(min)){
    exclude[exclude < min] <- 0
  }
  
  if (!is.na(max)){
    exclude[exclude > max] <- 0
  }
  return(exclude)
}

# Custom exclusion ----
## Add here, if necessary


# Create map ----
exclusion <- fn.exclude_depth(basemap,exclusion,min_depth,max_depth)

writeRaster(exclusion,file.path(indir,"exclusions.asc"),overwrite= T)


# Remove excess files created from writeRaster ----
aux_files <- c(
  gsub("\\.asc$", ".prj", indir, ignore.case = TRUE),
  gsub("\\.asc$", ".prg", indir, ignore.case = TRUE),
  gsub("\\.asc$", ".aux", indir, ignore.case = TRUE),
  gsub("\\.asc$", ".asc.aux.xml", indir, ignore.case = TRUE)
)

# Delete them if they were created
for (af in aux_files) {
  if (file.exists(af)) {
    file.remove(af)
  }
}
