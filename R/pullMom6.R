library(ncdf4)
library(terra)

# Hindcast ----
## tos ----
# 1. Setup paths
output_dir <- "../EnvironmentalData/MOM6Hindcast/tos"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

template_rast <- rast("../bathymetry.asc")
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# 2. Connect to OPeNDAP
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/hindcast/monthly/regrid/r20250715/tos.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"
nc <- nc_open(url)

# 3. Setup dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_vals <- ncvar_get(nc, "time") 
n_months <- length(time_vals)
dates <- seq(as.Date("1993-01-01"), by = "month", length.out = n_months)

lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))

# 4. Batch Loop
for (m in 1:n_months) {
  curr_date <- format(dates[m], "%Y-%m")
  file_path <- file.path(output_dir, paste0("tos_", curr_date, ".asc"))
  
  if (file.exists(file_path)) next
  
  message(paste("Processing:", curr_date))
  
  tos_array <- ncvar_get(nc, "tos", start = c(min(lon_idx), min(lat_idx), m), count = c(length(lon_idx), length(lat_idx), 1))
  
  mom6_remote_rast <- rast(t(tos_array), extent = remote_extent, crs = "EPSG:4326")
  mom6_remote_rast <- flip(mom6_remote_rast, direction = "vertical")
  mom6_final <- resample(mom6_remote_rast, template_rast, method = "bilinear")
  
  mom6_final[is.na(mom6_final) | mom6_final == 0] <- NA
  
  mom6_filled <- mom6_final
  for (i in 1:20) {
    mom6_filled <- focal(mom6_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  if (any(is.na(values(mom6_filled)))) {
    mom6_filled <- focal(mom6_filled, w = 51, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  # 5. Save the .asc file exclusively
  # 'gdal=NULL' prevents extra metadata creation
  writeRaster(mom6_filled, file_path, overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S",
              gdal = c("CREATE_PRJ=NO")) 
  
  # 6. Explicit Cleanup of unwanted sidecar files
  # Some GDAL versions ignore the flag above, so we delete them manually
  file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  file.remove(gsub(".asc", ".prj", file_path), showWarnings = FALSE)
  file.remove(paste0(file_path, ".xml"), showWarnings = FALSE)
}

nc_close(nc)
message("Done. Only .asc files remain in ", output_dir)

## sos ----
# 1. Setup paths
output_dir <- "../EnvironmentalData/MOM6Hindcast/sos"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

template_rast <- rast("../bathymetry.asc")
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# 2. Connect to OPeNDAP
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/hindcast/monthly/regrid/r20250715/sos.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"
nc <- nc_open(url)

# 3. Setup dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_vals <- ncvar_get(nc, "time") 
n_months <- length(time_vals)
dates <- seq(as.Date("1993-01-01"), by = "month", length.out = n_months)

lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))

# 4. Batch Loop
for (m in 1:n_months) {
  curr_date <- format(dates[m], "%Y-%m")
  file_path <- file.path(output_dir, paste0("sos_", curr_date, ".asc"))
  
  if (file.exists(file_path)) next
  
  message(paste("Processing:", curr_date))
  
  tos_array <- ncvar_get(nc, "sos", start = c(min(lon_idx), min(lat_idx), m), count = c(length(lon_idx), length(lat_idx), 1))
  
  mom6_remote_rast <- rast(t(tos_array), extent = remote_extent, crs = "EPSG:4326")
  mom6_remote_rast <- flip(mom6_remote_rast, direction = "vertical")
  mom6_final <- resample(mom6_remote_rast, template_rast, method = "bilinear")
  
  mom6_final[is.na(mom6_final) | mom6_final == 0] <- NA
  
  mom6_filled <- mom6_final
  for (i in 1:20) {
    mom6_filled <- focal(mom6_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  if (any(is.na(values(mom6_filled)))) {
    mom6_filled <- focal(mom6_filled, w = 51, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  # 5. Save the .asc file exclusively
  # 'gdal=NULL' prevents extra metadata creation
  writeRaster(mom6_filled, file_path, overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S",
              gdal = c("CREATE_PRJ=NO")) 
  
  # 6. Explicit Cleanup of unwanted sidecar files
  # Some GDAL versions ignore the flag above, so we delete them manually
  file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  file.remove(gsub(".asc", ".prj", file_path), showWarnings = FALSE)
  file.remove(paste0(file_path, ".xml"), showWarnings = FALSE)
}

nc_close(nc)
message("Done. Only .asc files remain in ", output_dir)

## sob ----
# 1. Setup paths
output_dir <- "../EnvironmentalData/MOM6Hindcast/sob"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

template_rast <- rast("../bathymetry.asc")
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# 2. Connect to OPeNDAP
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/hindcast/monthly/regrid/r20250715/sob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"
nc <- nc_open(url)

# 3. Setup dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_vals <- ncvar_get(nc, "time") 
n_months <- length(time_vals)
dates <- seq(as.Date("1993-01-01"), by = "month", length.out = n_months)

lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))

# 4. Batch Loop
for (m in 1:n_months) {
  curr_date <- format(dates[m], "%Y-%m")
  file_path <- file.path(output_dir, paste0("sob_", curr_date, ".asc"))
  
  if (file.exists(file_path)) next
  
  message(paste("Processing:", curr_date))
  
  tos_array <- ncvar_get(nc, "sob", start = c(min(lon_idx), min(lat_idx), m), count = c(length(lon_idx), length(lat_idx), 1))
  
  mom6_remote_rast <- rast(t(tos_array), extent = remote_extent, crs = "EPSG:4326")
  mom6_remote_rast <- flip(mom6_remote_rast, direction = "vertical")
  mom6_final <- resample(mom6_remote_rast, template_rast, method = "bilinear")
  
  mom6_final[is.na(mom6_final) | mom6_final == 0] <- NA
  
  mom6_filled <- mom6_final
  for (i in 1:20) {
    mom6_filled <- focal(mom6_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  if (any(is.na(values(mom6_filled)))) {
    mom6_filled <- focal(mom6_filled, w = 51, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  # 5. Save the .asc file exclusively
  # 'gdal=NULL' prevents extra metadata creation
  writeRaster(mom6_filled, file_path, overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S",
              gdal = c("CREATE_PRJ=NO")) 
  
  # 6. Explicit Cleanup of unwanted sidecar files
  # Some GDAL versions ignore the flag above, so we delete them manually
  file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  file.remove(gsub(".asc", ".prj", file_path), showWarnings = FALSE)
  file.remove(paste0(file_path, ".xml"), showWarnings = FALSE)
}

nc_close(nc)
message("Done. Only .asc files remain in ", output_dir)

#NPP
# 1. Setup paths
output_dir <- "../EnvironmentalData/MOM6Hindcast/npp"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

template_rast <- rast("../bathymetry.asc")
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# 2. Connect to OPeNDAP (NPP URL)
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/hindcast/monthly/regrid/r20250715/wc_vert_int_npp.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"
nc <- nc_open(url)

# 3. Setup dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_vals <- ncvar_get(nc, "time") 
n_months <- length(time_vals)
dates <- seq(as.Date("1993-01-01"), by = "month", length.out = n_months)

lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))

# 4. Batch Loop
for (m in 1:n_months) {
  curr_date <- format(dates[m], "%Y-%m")
  file_path <- file.path(output_dir, paste0("npp_", curr_date, ".asc"))
  
  if (file.exists(file_path)) next
  
  message(paste("Processing NPP:", curr_date))
  
  # Note: The variable name for vertically integrated NPP is typically 'intpp'
  npp_array <- ncvar_get(nc, "wc_vert_int_npp", start = c(min(lon_idx), min(lat_idx), m), count = c(length(lon_idx), length(lat_idx), 1))
  
  mom6_remote_rast <- rast(t(npp_array), extent = remote_extent, crs = "EPSG:4326")
  mom6_remote_rast <- flip(mom6_remote_rast, direction = "vertical")
  mom6_final <- resample(mom6_remote_rast, template_rast, method = "bilinear")
  
  # Remove land/NoData for scaling calculation
  mom6_final[mom6_final <= 0] <- NA
  
  # --- SCALING LOGIC ---
  # Calculate the mean of the valid ocean cells for this month
  avg_npp <- global(mom6_final, "mean", na.rm = TRUE)$mean
  
  # Scale and center around 1: divide every cell by the mean
  # If avg_npp is 0 (unlikely for NPP), keep as is to avoid division by zero
  if (!is.na(avg_npp) && avg_npp > 0) {
    mom6_final <- mom6_final / avg_npp
  }
  # ---------------------
  
  # Gap filling for the shoreline
  mom6_filled <- mom6_final
  for (i in 1:20) {
    mom6_filled <- focal(mom6_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  if (any(is.na(values(mom6_filled)))) {
    mom6_filled <- focal(mom6_filled, w = 51, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  # 5. Save the .asc file
  writeRaster(mom6_filled, file_path, overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S",
              gdal = c("CREATE_PRJ=NO")) 
  
  # 6. Explicit Cleanup
  file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  file.remove(gsub(".asc", ".prj", file_path), showWarnings = FALSE)
  file.remove(paste0(file_path, ".xml"), showWarnings = FALSE)
}

nc_close(nc)
message("NPP processing complete.")

## tob ----
# 1. Setup paths
output_dir <- "../EnvironmentalData/MOM6Hindcast/tob"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

template_rast <- rast("../bathymetry.asc")
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# 2. Connect to OPeNDAP
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/hindcast/monthly/regrid/r20250715/tob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"
nc <- nc_open(url)

# 3. Setup dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_vals <- ncvar_get(nc, "time") 
n_months <- length(time_vals)
dates <- seq(as.Date("1993-01-01"), by = "month", length.out = n_months)

lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))

# 4. Batch Loop
for (m in 1:n_months) {
  curr_date <- format(dates[m], "%Y-%m")
  file_path <- file.path(output_dir, paste0("tob_", curr_date, ".asc"))
  
  if (file.exists(file_path)) next
  
  message(paste("Processing:", curr_date))
  
  tos_array <- ncvar_get(nc, "tob", start = c(min(lon_idx), min(lat_idx), m), count = c(length(lon_idx), length(lat_idx), 1))
  
  mom6_remote_rast <- rast(t(tos_array), extent = remote_extent, crs = "EPSG:4326")
  mom6_remote_rast <- flip(mom6_remote_rast, direction = "vertical")
  mom6_final <- resample(mom6_remote_rast, template_rast, method = "bilinear")
  
  mom6_final[is.na(mom6_final) | mom6_final == 0] <- NA
  
  mom6_filled <- mom6_final
  for (i in 1:20) {
    mom6_filled <- focal(mom6_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  if (any(is.na(values(mom6_filled)))) {
    mom6_filled <- focal(mom6_filled, w = 51, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  # 5. Save the .asc file exclusively
  # 'gdal=NULL' prevents extra metadata creation
  writeRaster(mom6_filled, file_path, overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S",
              gdal = c("CREATE_PRJ=NO")) 
  
  # 6. Explicit Cleanup of unwanted sidecar files
  # Some GDAL versions ignore the flag above, so we delete them manually
  file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  file.remove(gsub(".asc", ".prj", file_path), showWarnings = FALSE)
  file.remove(paste0(file_path, ".xml"), showWarnings = FALSE)
}

nc_close(nc)
message("Done. Only .asc files remain in ", output_dir)

## sob ----
# 1. Setup paths
output_dir <- "../EnvironmentalData/MOM6Hindcast/sob"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

template_rast <- rast("../bathymetry.asc")
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# 2. Connect to OPeNDAP
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/hindcast/monthly/regrid/r20250715/sob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"
nc <- nc_open(url)

# 3. Setup dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_vals <- ncvar_get(nc, "time") 
n_months <- length(time_vals)
dates <- seq(as.Date("1993-01-01"), by = "month", length.out = n_months)

lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))

# 4. Batch Loop
for (m in 1:n_months) {
  curr_date <- format(dates[m], "%Y-%m")
  file_path <- file.path(output_dir, paste0("tob_", curr_date, ".asc"))
  
  if (file.exists(file_path)) next
  
  message(paste("Processing:", curr_date))
  
  tos_array <- ncvar_get(nc, "sob", start = c(min(lon_idx), min(lat_idx), m), count = c(length(lon_idx), length(lat_idx), 1))
  
  mom6_remote_rast <- rast(t(tos_array), extent = remote_extent, crs = "EPSG:4326")
  mom6_remote_rast <- flip(mom6_remote_rast, direction = "vertical")
  mom6_final <- resample(mom6_remote_rast, template_rast, method = "bilinear")
  
  mom6_final[is.na(mom6_final) | mom6_final == 0] <- NA
  
  mom6_filled <- mom6_final
  for (i in 1:20) {
    mom6_filled <- focal(mom6_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  if (any(is.na(values(mom6_filled)))) {
    mom6_filled <- focal(mom6_filled, w = 51, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  # 5. Save the .asc file exclusively
  # 'gdal=NULL' prevents extra metadata creation
  writeRaster(mom6_filled, file_path, overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S",
              gdal = c("CREATE_PRJ=NO")) 
  
  # 6. Explicit Cleanup of unwanted sidecar files
  # Some GDAL versions ignore the flag above, so we delete them manually
  file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  file.remove(gsub(".asc", ".prj", file_path), showWarnings = FALSE)
  file.remove(paste0(file_path, ".xml"), showWarnings = FALSE)
}

nc_close(nc)
message("Done. Only .asc files remain in ", output_dir)

#Decadal Forecast ----
## tos ----
# 1. Setup paths
output_dir <- "./EnvironmentalData/MOM6Forecast/tos"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

template_rast <- rast("South_Florida_Positive_Depth.asc")
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# 2. Connect to OPeNDAP
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/decadal_forecast/monthly/regrid/latest/tos.nwa.full.dc_fcast.monthly.regrid.r20250925.enss.i202401.nc"
nc <- nc_open(url)

print(nc)
# 3. Setup dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_vals <- ncvar_get(nc, "lead") 
n_months <- length(time_vals)
dates <- seq(as.Date("2024-01-01"), by = "month", length.out = n_months)

lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))

# 4. Batch Loop
for (m in 1:n_months) {
  curr_date <- format(dates[m], "%Y-%m")
  file_path <- file.path(output_dir, paste0("tos_", curr_date, ".asc"))
  
  if (file.exists(file_path)) next
  
  message(paste("Processing:", curr_date))
  
  tos_array <- ncvar_get(nc, "tos", start = c(min(lon_idx), min(lat_idx), m,1), count = c(length(lon_idx), length(lat_idx), 1,1))
  
  mom6_remote_rast <- rast(t(tos_array), extent = remote_extent, crs = "EPSG:4326")
  mom6_remote_rast <- flip(mom6_remote_rast, direction = "vertical")
  mom6_final <- resample(mom6_remote_rast, template_rast, method = "bilinear")
  
  mom6_final[is.na(mom6_final) | mom6_final == 0] <- NA
  
  mom6_filled <- mom6_final
  for (i in 1:20) {
    mom6_filled <- focal(mom6_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  if (any(is.na(values(mom6_filled)))) {
    mom6_filled <- focal(mom6_filled, w = 51, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  # 5. Save the .asc file exclusively
  # 'gdal=NULL' prevents extra metadata creation
  writeRaster(mom6_filled, file_path, overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S",
              gdal = c("CREATE_PRJ=NO")) 
  
  # 6. Explicit Cleanup of unwanted sidecar files
  # Some GDAL versions ignore the flag above, so we delete them manually
  file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  file.remove(gsub(".asc", ".prj", file_path), showWarnings = FALSE)
  file.remove(paste0(file_path, ".xml"), showWarnings = FALSE)
}

nc_close(nc)
message("Done. Only .asc files remain in ", output_dir)

## sos ----
# 1. Setup paths
output_dir <- "./EnvironmentalData/MOM6Forecast/sos"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

template_rast <- rast("South_Florida_Positive_Depth.asc")
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# 2. Connect to OPeNDAP
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/decadal_forecast/monthly/regrid/latest/sos.nwa.full.dc_fcast.monthly.regrid.r20250925.enss.i202401.nc"
nc <- nc_open(url)

# 3. Setup dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_vals <- ncvar_get(nc, "lead") 
n_months <- length(time_vals)
dates <- seq(as.Date("2024-01-01"), by = "month", length.out = n_months)

lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))

# 4. Batch Loop
for (m in 1:n_months) {
  curr_date <- format(dates[m], "%Y-%m")
  file_path <- file.path(output_dir, paste0("sos_", curr_date, ".asc"))
  
  if (file.exists(file_path)) next
  
  message(paste("Processing:", curr_date))
  
  tos_array <- ncvar_get(nc, "sos", start = c(min(lon_idx), min(lat_idx), m,1), count = c(length(lon_idx), length(lat_idx), 1,1))
  
  mom6_remote_rast <- rast(t(tos_array), extent = remote_extent, crs = "EPSG:4326")
  mom6_remote_rast <- flip(mom6_remote_rast, direction = "vertical")
  mom6_final <- resample(mom6_remote_rast, template_rast, method = "bilinear")
  
  mom6_final[is.na(mom6_final) | mom6_final == 0] <- NA
  
  mom6_filled <- mom6_final
  for (i in 1:20) {
    mom6_filled <- focal(mom6_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  if (any(is.na(values(mom6_filled)))) {
    mom6_filled <- focal(mom6_filled, w = 51, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  # 5. Save the .asc file exclusively
  # 'gdal=NULL' prevents extra metadata creation
  writeRaster(mom6_filled, file_path, overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S",
              gdal = c("CREATE_PRJ=NO")) 
  
  # 6. Explicit Cleanup of unwanted sidecar files
  # Some GDAL versions ignore the flag above, so we delete them manually
  file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  file.remove(gsub(".asc", ".prj", file_path), showWarnings = FALSE)
  file.remove(paste0(file_path, ".xml"), showWarnings = FALSE)
}

nc_close(nc)
message("Done. Only .asc files remain in ", output_dir)

#NPP
# 1. Setup paths
output_dir <- "./EnvironmentalData/MOM6Forecast/npp"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

template_rast <- rast("South_Florida_Positive_Depth.asc")
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# 2. Connect to OPeNDAP (NPP URL)
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/decadal_forecast/monthly/regrid/latest/wc_vert_int_npp.nwa.full.dc_fcast.monthly.regrid.r20250925.enss.i202401.nc"
nc <- nc_open(url)

# 3. Setup dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_vals <- ncvar_get(nc, "lead") 
n_months <- length(time_vals)
dates <- seq(as.Date("2024-01-01"), by = "month", length.out = n_months)

lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))

# 4. Batch Loop
for (m in 1:n_months) {
  curr_date <- format(dates[m], "%Y-%m")
  file_path <- file.path(output_dir, paste0("npp_", curr_date, ".asc"))
  
  if (file.exists(file_path)) next
  
  message(paste("Processing NPP:", curr_date))
  
  # Note: The variable name for vertically integrated NPP is typically 'intpp'
  npp_array <- ncvar_get(nc, "wc_vert_int_npp", start = c(min(lon_idx), min(lat_idx), m,1), count = c(length(lon_idx), length(lat_idx), 1,1))
  
  mom6_remote_rast <- rast(t(npp_array), extent = remote_extent, crs = "EPSG:4326")
  mom6_remote_rast <- flip(mom6_remote_rast, direction = "vertical")
  mom6_final <- resample(mom6_remote_rast, template_rast, method = "bilinear")
  
  # Remove land/NoData for scaling calculation
  mom6_final[mom6_final <= 0] <- NA
  
  # --- SCALING LOGIC ---
  # Calculate the mean of the valid ocean cells for this month
  avg_npp <- global(mom6_final, "mean", na.rm = TRUE)$mean
  
  # Scale and center around 1: divide every cell by the mean
  # If avg_npp is 0 (unlikely for NPP), keep as is to avoid division by zero
  if (!is.na(avg_npp) && avg_npp > 0) {
    mom6_final <- mom6_final / avg_npp
  }
  # ---------------------
  
  # Gap filling for the shoreline
  mom6_filled <- mom6_final
  for (i in 1:20) {
    mom6_filled <- focal(mom6_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  if (any(is.na(values(mom6_filled)))) {
    mom6_filled <- focal(mom6_filled, w = 51, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  # 5. Save the .asc file
  writeRaster(mom6_filled, file_path, overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S",
              gdal = c("CREATE_PRJ=NO")) 
  
  # 6. Explicit Cleanup
  file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  file.remove(gsub(".asc", ".prj", file_path), showWarnings = FALSE)
  file.remove(paste0(file_path, ".xml"), showWarnings = FALSE)
}

nc_close(nc)
message("NPP processing complete.")

#tob
# 1. Setup paths
output_dir <- "../EnvironmentalData/MOM6Forecast/tob"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

template_rast <- rast("../bathymetry.asc")
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# 2. Connect to OPeNDAP (NPP URL)
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/decadal_forecast/monthly/regrid/r20250925/tob.nwa.full.dc_fcast.monthly.regrid.r20250925.enss.i202401.nc"
nc <- nc_open(url)

# 3. Setup dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_vals <- ncvar_get(nc, "lead") 
n_months <- length(time_vals)
dates <- seq(as.Date("2024-01-01"), by = "month", length.out = n_months)

lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))

# 4. Batch Loop
for (m in 1:n_months) {
  curr_date <- format(dates[m], "%Y-%m")
  file_path <- file.path(output_dir, paste0("tob_", curr_date, ".asc"))
  
  if (file.exists(file_path)) next
  
  message(paste("Processing TOB:", curr_date))
  
  # Note: The variable name for vertically integrated NPP is typically 'intpp'
  npp_array <- ncvar_get(nc, "tob", start = c(min(lon_idx), min(lat_idx), m,1), count = c(length(lon_idx), length(lat_idx), 1,1))
  
  mom6_remote_rast <- rast(t(npp_array), extent = remote_extent, crs = "EPSG:4326")
  mom6_remote_rast <- flip(mom6_remote_rast, direction = "vertical")
  mom6_final <- resample(mom6_remote_rast, template_rast, method = "bilinear")
  
  # Remove land/NoData for scaling calculation
  mom6_final[mom6_final <= 0] <- NA
  
  # Gap filling for the shoreline
  mom6_filled <- mom6_final
  for (i in 1:20) {
    mom6_filled <- focal(mom6_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  if (any(is.na(values(mom6_filled)))) {
    mom6_filled <- focal(mom6_filled, w = 51, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  # 5. Save the .asc file
  writeRaster(mom6_filled, file_path, overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S",
              gdal = c("CREATE_PRJ=NO")) 
  
  # 6. Explicit Cleanup
  file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  file.remove(gsub(".asc", ".prj", file_path), showWarnings = FALSE)
  file.remove(paste0(file_path, ".xml"), showWarnings = FALSE)
}

nc_close(nc)
message("tob processing complete.")

#sob
# 1. Setup paths
output_dir <- "../EnvironmentalData/MOM6Forecast/sob"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

template_rast <- rast("../bathymetry.asc")
crs(template_rast) <- "EPSG:4326"
local_ext <- ext(template_rast)

# 2. Connect to OPeNDAP (NPP URL)
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/decadal_forecast/monthly/regrid/r20250925/sob.nwa.full.dc_fcast.monthly.regrid.r20250925.enss.i202401.nc"

nc <- nc_open(url)

# 3. Setup dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_vals <- ncvar_get(nc, "lead") 
n_months <- length(time_vals)
dates <- seq(as.Date("2024-01-01"), by = "month", length.out = n_months)

lon_idx <- which(lon >= (local_ext$xmin - 0.5) & lon <= (local_ext$xmax + 0.5))
lat_idx <- which(lat >= (local_ext$ymin - 0.5) & lat <= (local_ext$ymax + 0.5))
remote_extent <- ext(min(lon[lon_idx]), max(lon[lon_idx]), min(lat[lat_idx]), max(lat[lat_idx]))

# 4. Batch Loop
for (m in 1:n_months) {
  curr_date <- format(dates[m], "%Y-%m")
  file_path <- file.path(output_dir, paste0("sob_", curr_date, ".asc"))
  
  if (file.exists(file_path)) next
  
  message(paste("Processing SOB:", curr_date))
  
  # Note: The variable name for vertically integrated NPP is typically 'intpp'
  npp_array <- ncvar_get(nc, "sob", start = c(min(lon_idx), min(lat_idx), m,1), count = c(length(lon_idx), length(lat_idx), 1,1))
  
  mom6_remote_rast <- rast(t(npp_array), extent = remote_extent, crs = "EPSG:4326")
  mom6_remote_rast <- flip(mom6_remote_rast, direction = "vertical")
  mom6_final <- resample(mom6_remote_rast, template_rast, method = "bilinear")
  
  # Remove land/NoData for scaling calculation
  mom6_final[mom6_final <= 0] <- NA
  
  # Gap filling for the shoreline
  mom6_filled <- mom6_final
  for (i in 1:20) {
    mom6_filled <- focal(mom6_filled, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  if (any(is.na(values(mom6_filled)))) {
    mom6_filled <- focal(mom6_filled, w = 51, fun = "mean", na.rm = TRUE, na.policy = "only")
  }
  
  # 5. Save the .asc file
  writeRaster(mom6_filled, file_path, overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S",
              gdal = c("CREATE_PRJ=NO")) 
  
  # 6. Explicit Cleanup
  file.remove(paste0(file_path, ".aux.xml"), showWarnings = FALSE)
  file.remove(gsub(".asc", ".prj", file_path), showWarnings = FALSE)
  file.remove(paste0(file_path, ".xml"), showWarnings = FALSE)
}

nc_close(nc)
message("sob processing complete.")
