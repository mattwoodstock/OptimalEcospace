library(ncdf4)
library(terra)

# 1. Setup Master Template
template_rast <- rast("./Outputs-for-ewe/Seabed/sedcomp-gravel.asc") 
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# Variable names to process
vars <- c("tos", "sos", "sob", "tob", "wc_vert_int_npp")
start_year <- 2009  # Set the start year here

for (var_name in vars) {
  output_dir <- file.path("./Outputs-for-ewe/EnvironmentalData/MOM6Hindcast", var_name)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # URL Construction 
  url <- paste0("http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/hindcast/monthly/regrid/r20250715/", 
                if(var_name == "wc_vert_int_npp") "wc_vert_int_npp" else var_name, 
                ".nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc")
  
  nc <- nc_open(url)
  lon <- ncvar_get(nc, "lon"); lat <- ncvar_get(nc, "lat")
  
  # Create the full date sequence from the NC file metadata (starts at 1993-01)
  all_dates <- seq(as.Date("1993-01-01"), by = "month", length.out = length(ncvar_get(nc, "time")))
  
  # Identify indices where year >= 2009
  target_indices <- which(as.numeric(format(all_dates, "%Y")) >= start_year)
  
  # Buffer indices for interpolation
  lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
  lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
  remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))
  
  # Loop only through the target indices
  for (m in target_indices) {
    curr_date_obj <- all_dates[m]
    curr_date <- format(curr_date_obj, "%Y-%m")
    file_path <- file.path(output_dir, paste0(var_name, "_", curr_date, ".asc"))
    
    if (file.exists(file_path)) next
    
    message(paste("Processing", var_name, ":", curr_date))
    
    # Extract data using 'm' as the time index
    data_array <- ncvar_get(nc, var_name, 
                            start = c(min(lon_idx), min(lat_idx), m), 
                            count = c(length(lon_idx), length(lat_idx), 1))
    
    r <- rast(t(data_array), extent = remote_extent, crs = "EPSG:4326")
    r <- flip(r, direction = "vertical")
    
    # 3. Clean coarse data
    r[r <= 0 | r == -9999] <- NA 
    
    # 4. Fill NAs (Coastal "push")
    r_filled <- focal(r, w = 5, fun = "mean", na.rm = TRUE, na.policy = "all")
    for (i in 1:5) {
      r_filled <- focal(r_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
    }
    
    # 5. Resample and Mask
    r_resampled <- resample(r_filled, template_rast, method = "bilinear")
    final_r <- mask(r_resampled, template_rast)
    
    # 6. NPP Scaling
    if(var_name == "wc_vert_int_npp") {
      avg_npp <- global(final_r, "mean", na.rm = TRUE)$mean
      if (!is.na(avg_npp) && avg_npp > 0) final_r <- final_r / avg_npp
    }
    
    # 7. Write output
    writeRaster(final_r, file_path, overwrite = TRUE, 
                NAflag = -9999, datatype = "FLT4S", 
                gdal = c("CREATE_PRJ=NO"))
    
    file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  }
  nc_close(nc)
}

#### Decadal Forecast ----
for (var_name in vars) {
  output_dir <- file.path("./Outputs-for-ewe/EnvironmentalData/MOM6Forecast", var_name)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # URL Construction 
  url <- paste0("http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/decadal_forecast/monthly/regrid/latest/", 
                if(var_name == "wc_vert_int_npp") "wc_vert_int_npp" else var_name, 
                ".nwa.full.dc_fcast.monthly.regrid.r20250925.enss.i202401.nc")
  
  nc <- nc_open(url)
  lon <- ncvar_get(nc, "lon"); lat <- ncvar_get(nc, "lat")
  
  # Create the full date sequence from the NC file metadata (starts at 1993-01)
  all_dates <- seq(as.Date("2024-01-01"), by = "month", length.out = length(ncvar_get(nc, "lead")))
  
  # Identify indices where year >= 2009
  target_indices <- which(as.numeric(format(all_dates, "%Y")) >= start_year)
  
  # Buffer indices for interpolation
  lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
  lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
  remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))
  
  # Loop only through the target indices
  for (m in target_indices) {
    curr_date_obj <- all_dates[m]
    curr_date <- format(curr_date_obj, "%Y-%m")
    file_path <- file.path(output_dir, paste0(var_name, "_", curr_date, ".asc"))
    
    if (file.exists(file_path)) next
    
    message(paste("Processing", var_name, ":", curr_date))
    
    # Extract data using 'm' as the time index
    data_array <- ncvar_get(nc, var_name, 
                            start = c(min(lon_idx), min(lat_idx), m,1), 
                            count = c(length(lon_idx), length(lat_idx), 1,1))
    
    r <- rast(t(data_array), extent = remote_extent, crs = "EPSG:4326")
    r <- flip(r, direction = "vertical")
    
    # 3. Clean coarse data
    r[r <= 0 | r == -9999] <- NA 
    
    # 4. Fill NAs (Coastal "push")
    r_filled <- focal(r, w = 5, fun = "mean", na.rm = TRUE, na.policy = "all")
    for (i in 1:5) {
      r_filled <- focal(r_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
    }
    
    # 5. Resample and Mask
    r_resampled <- resample(r_filled, template_rast, method = "bilinear")
    final_r <- mask(r_resampled, template_rast)
    
    # 6. NPP Scaling
    if(var_name == "wc_vert_int_npp") {
      avg_npp <- global(final_r, "mean", na.rm = TRUE)$mean
      if (!is.na(avg_npp) && avg_npp > 0) final_r <- final_r / avg_npp
    }
    
    # 7. Write output
    writeRaster(final_r, file_path, overwrite = TRUE, 
                NAflag = -9999, datatype = "FLT4S", 
                gdal = c("CREATE_PRJ=NO"))
    
    file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  }
  nc_close(nc)
}
