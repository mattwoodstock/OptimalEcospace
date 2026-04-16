library(ncdf4)
library(terra)

# Setup paths and Template
# Using the basemap as the strict mask for the final output
template_rast <- rast("./Outputs-for-ewe/Seabed/sedcomp-gravel.asc") 
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# Define variables to process
vars <- c("tos", "sos", "sob", "tob", "wc_vert_int_npp")

for (var_name in vars) {
  # Handle directory logic
  output_dir <- file.path("./Outputs-for-ewe/EnvironmentalData/MOM6Hindcast", var_name)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Construct URL (standardizing for hindcast as an example)
  # Note: NPP uses a different filename structure in your source
  if(var_name == "wc_vert_int_npp") {
    url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/hindcast/monthly/regrid/r20250715/wc_vert_int_npp.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"
  } else {
    url <- paste0("http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/hindcast/monthly/regrid/r20250715/", var_name, ".nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc")
  }
  
  nc <- nc_open(url)
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  time_vals <- ncvar_get(nc, "time")
  n_months <- length(time_vals)
  dates <- seq(as.Date("1993-01-01"), by = "month", length.out = n_months)
  
  lon_idx <- which(lon >= (local_ext$xmin - 1) & lon <= (local_ext$xmax + 1))
  lat_idx <- which(lat >= (local_ext$ymin - 1) & lat <= (local_ext$ymax + 1))
  remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))
  
  for (m in 1:n_months) {
    curr_date <- format(dates[m], "%Y-%m")
    file_path <- file.path(output_dir, paste0(var_name, "_", curr_date, ".asc"))
    if (file.exists(file_path)) next
    
    message(paste("Processing", var_name, ":", curr_date))
    
    # Extract data
    data_array <- ncvar_get(nc, var_name, start = c(min(lon_idx), min(lat_idx), m), count = c(length(lon_idx), length(lat_idx), 1))
    
    # Create initial raster
    r <- rast(t(data_array), extent = remote_extent, crs = "EPSG:4326")
    r <- flip(r, direction = "vertical")
    
    # 1. Handle NoData before resampling to prevent "smearing" 0s into the ocean
    r[r <= 0] <- NA 
    
    # 2. Resample to template resolution
    r_interp <- resample(r, template_rast, method = "bilinear")
    
    # 3. Aggressive Gap Filling 
    # This fills beyond the coastline to ensure every pixel in the template is covered
    filled <- r_interp
    for (i in 1:30) {
      filled <- focal(filled, w = 3, fun = mean, na.rm = TRUE, na.policy = "only")
    }
    
    # 4. STRICT MASKING
    # This cuts the 'bleeding' edges exactly to your basemap's land-sea boundary [cite: 1]
    final_r <- mask(filled, template_rast)
    
    # 5. NPP Scaling Logic [cite: 2]
    if(var_name == "wc_vert_int_npp") {
      avg_npp <- global(final_r, "mean", na.rm = TRUE)$mean
      if (!is.na(avg_npp) && avg_npp > 0) final_r <- final_r / avg_npp
    }
    
    # Write output
    writeRaster(final_r, file_path, overwrite = TRUE, 
                NAflag = -9999, datatype = "FLT4S",
                gdal = c("CREATE_PRJ=NO"))
    
    # Cleanup sidecars
    file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  }
  nc_close(nc)
}