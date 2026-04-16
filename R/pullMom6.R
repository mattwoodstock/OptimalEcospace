library(ncdf4)
library(terra)

# 1. Setup Master Template (The strict mask for Ecospace)
template_rast <- rast("./Outputs-for-ewe/Seabed/sedcomp-gravel.asc") 
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# Variable names to process
vars <- c("tos", "sos", "sob", "tob", "wc_vert_int_npp")

for (var_name in vars) {
  output_dir <- file.path("./Outputs-for-ewe/EnvironmentalData/MOM6Hindcast", var_name)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # URL Construction 
  url <- paste0("http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/hindcast/monthly/regrid/r20250715/", 
                if(var_name == "wc_vert_int_npp") "wc_vert_int_npp" else var_name, 
                ".nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc")
  
  nc <- nc_open(url)
  lon <- ncvar_get(nc, "lon"); lat <- ncvar_get(nc, "lat")
  dates <- seq(as.Date("1993-01-01"), by = "month", length.out = length(ncvar_get(nc, "time")))
  
  # Buffer indices to ensure data is available beyond the edges for interpolation 
  lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
  lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
  remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))
  
  for (m in 1:length(dates)) {
    curr_date <- format(dates[m], "%Y-%m")
    file_path <- file.path(output_dir, paste0(var_name, "_", curr_date, ".asc"))
    if (file.exists(file_path)) next
    
    message(paste("Processing", var_name, ":", curr_date))
    
    # 2. Extract coarse MOM6 data 
    data_array <- ncvar_get(nc, var_name, start = c(min(lon_idx), min(lat_idx), m), count = c(length(lon_idx), length(lat_idx), 1))
    r <- rast(t(data_array), extent = remote_extent, crs = "EPSG:4326")
    r <- flip(r, direction = "vertical") [cite: 1]
    
    # 3. Clean coarse data: Replace 0, artifact (16.68...), and NoData (-9999) with NA [cite: 1, 32]
    # This prevents these values from "smearing" into the ocean during resampling
    r[r <= 0 | r == -9999] <- NA 
    
    # 4. Fill ALL NAs by extending existing non-zero values 
    # This "pushes" the ocean data into the coastal gaps
    r_filled <- focal(r, w = 5, fun = "mean", na.rm = TRUE, na.policy = "all")
    for (i in 1:5) {
      r_filled <- focal(r_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
    }
    
    # 5. Resample to high-res template and apply strict mask 
    r_resampled <- resample(r_filled, template_rast, method = "bilinear")
    final_r <- mask(r_resampled, template_rast) [cite: 1]
    
    # 6. NPP Scaling Logic 
    if(var_name == "wc_vert_int_npp") {
      avg_npp <- global(final_r, "mean", na.rm = TRUE)$mean
      if (!is.na(avg_npp) && avg_npp > 0) final_r <- final_r / avg_npp
    }
    
    # 7. Write output for Ecospace 
    writeRaster(final_r, file_path, overwrite = TRUE, 
                NAflag = -9999, datatype = "FLT4S", 
                gdal = c("CREATE_PRJ=NO"))
    
    file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  }
  nc_close(nc)
}