# 1. Load required libraries
library(marmap)
library(terra)
library(ggplot2)
library(tidyterra)

# 2. Define spatial metadata from Chagaris et al. (2020)
# Footprint: 88.5-85.5W and 29.0-30.5N 
lat_max <- 30.5      
lat_min <- 29.0      
lon_min <- -88.5     
lon_max <- -85.5     

# Resolution for 1-arc-minute grid (1/60 of a degree)
res_deg <- 1/60 

# 3. Fetch the 1-arc-minute resolution bathymetry
# resolution = 1 pulls the high-res ETOPO data
bathy_data <- getNOAA.bathy(lon1 = lon_min, lon2 = lon_max, 
                            lat1 = lat_min, lat2 = lat_max, 
                            resolution = 1, keep = TRUE)

# 4. Convert to terra SpatRaster
bathy_xyz <- as.xyz(bathy_data)
fine_raster <- rast(bathy_xyz, type = "xyz", crs = "EPSG:4326")

# 5. Process Bathymetry for Ecospace (Ocean = Positive, Land = 0)
ecospace_depth <- fine_raster
ecospace_depth[ecospace_depth > 0] <- 0 
ecospace_depth <- abs(ecospace_depth)

# 6. Create the Binary Land Mask (1 = Water, 0 = Land)
land_mask <- fine_raster < 0

# 7. Save the .asc files for the Ecospace model
# Bathymetry file
writeRaster(ecospace_depth, 
            filename = "../Inputs/NGoM/1min_NGoM_bathymetry.asc", 
            filetype = "AAIGrid", 
            overwrite = TRUE, 
            NAflag = -9999)

# 8. Create the ggplot2 visualization
# To make land cells solid gray, we create a version of the depth raster
# where land cells (0 depth) are set to NA.
plot_depth <- ecospace_depth
plot_depth[land_mask == 0] <- NA

bathy_plot <- ggplot() +
  # Draw the bathymetry raster. Cells with NA (Land) will use na.value color.
  geom_spatraster(data = plot_depth) +
  scale_fill_viridis_c(
    name = "Depth (m)", 
    option = "mako", 
    direction = -1,
    na.value = "gray75" # This makes the entire land grid cell solid gray
  ) +
  # Styling and Labels
  labs(
    title = "NGoM 1-Arc-Minute Bathymetry & Land Mask",
    subtitle = "Domain: 88.5-85.5W, 29.0-30.5N (Chagaris et al. 2020)",
    caption = "Resolution: 1-arc-minute | 180 x 90 Cells | Solid Gray = Land Mask",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray95")
  )

# 9. Output the plot to the relative directory
ggsave(filename = "../Inputs/NGoM/NGoM_1min_Bathymetry_Map.png", 
       plot = bathy_plot, 
       width = 8, 
       height = 5, 
       dpi = 300)

# Verification
cat("Generated 180x90 grid for domain 29-30.5N, 88.5-85.5W.\n")
cat("Files saved to ../Inputs/NGoM/\n")