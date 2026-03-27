library(terra)

# 1. Load the original structures file
# (Assuming your original file is 'structures_orig.asc')
r <- rast("../Inputs/NGoM/structures.asc")

# 2. Define the target dimensions
target_ncols <- 180
target_nrows <- 90

# 3. Create a template with the EXACT same extent but new dimensions
# This ensures the geographic range does not shift.
template <- rast(ext(r), 
                 nrows = target_nrows, 
                 ncols = target_ncols, 
                 crs = crs(r))

# 4. Resample to the new 180x90 grid
# method = "near" ensures 0s and 1s stay 0s and 1s without interpolation.
structures_downscaled <- resample(r, template, method = "near")

# 5. Verify the dimensions
print(dim(structures_downscaled)) # Should show: 90 180 1

# 6. Save the output
writeRaster(structures_downscaled, "../Inputs/NGoM/structures.asc", 
            NAflag = -9999, overwrite = TRUE)
