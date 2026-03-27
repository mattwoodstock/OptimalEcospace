rm(list=ls()); graphics.off(); windows()

library(sf)
library(ggplot2)
library(terra)
library(raster)

dir_in  = "../Inputs/dbSeaBed"
dir_out = "./Outputs-for-ewe/Seabed/"
dir_fig = "./Figures/"

## Read in seabed
seabed  = sf::st_read(file.path(dir_in, "usSEABED_GOM_Sediments.shp"))

## Read in depth
depth = terra::rast("../Inputs/NGoM/basemap.asc")
plot(depth)


##------------------------------------------------------------------------------
## Seabed sediment composition 

## The attributes are denoted by the grid filenames: 
## gma_rckv - gridded values substrate rock presence (% exposure); computed with CS Interpolator; max 5km search radius 
## gma_rcku - gridded uncertainties on substrate rock presence (% exposure); 
## gma_gvlv - gridded values sediment gravel fraction content (% weight); computed with CS Interpolator; max 20km search radius 
## gma_gvlu - gridded uncertainties on sediment gravel contents (% weight); computed with CS Interpolator; max 20km search radius 
## gma_sndv - gridded values sediment sand fraction content (% weight); computed with CS Interpolator; max 20km search radius 
## gma_sndu - gridded uncertainties on sediment sand contents (% weight); computed with CS Interpolator; max 20km search radius 
## gma_mudv - gridded values sediment mud fraction content (% weight); computed with CS Interpolator; max 20km search radius 
## gma_mudu - gridded uncertainties on sediment mud contents (% weight); computed with CS Interpolator; max 20km search radius 
## gma_folk - gridded codes for sediment Folk Codes; the 0 (or " "),1,2,3 indicate negligible-, slightly-, x-ly-, or major components 
##            of mud, sand or gravel in code positions 1,2,3 respectively; they can be converted to "(x)xX" types codes; see the dbSEABED web sites for a suitable ESRI legend; 
##            computed from the foregoing mud, sand, gravel griddings. 
## gma_domnc - gridded codes for dominant types of substrates; 
##            the 0 (or " "),2,3 indicate negligible-, subdominant-, or dominant- components for rock, gravel, sand, mud 
##            in code positions 1,2,3,4 respectively; see the dbSEABED web sites for a suitable ESRI legend. 

## Check CRS and extent
terra::crs(depth)
sf::st_crs(seabed)

plot(depth, main = "Depth + seabed footprint")
plot(sf::st_geometry(seabed), add = TRUE, border = "red", lwd = 0.5)

## Fix depth
lat_max <- 30.5
lat_min <- 29.0
lon_min <- -88.5
lon_max <- -85.5

depth_fix <- depth
terra::ext(depth_fix) <- c(lon_min, lon_max, lat_min, lat_max)

plot(depth_fix, main = "Depth with corrected extent")
plot(sf::st_geometry(seabed), add = TRUE, border = "red", lwd = 0.5)

## Rasterize polygons
rckv <- terra::rasterize(seabed, depth_fix, field = "gom_rckv")
gvlv <- terra::rasterize(seabed, depth_fix, field = "gom_gvlv")
sndv <- terra::rasterize(seabed, depth_fix, field = "gom_sndv")
mudv <- terra::rasterize(seabed, depth_fix, field = "gom_mudv")

## Change -99 to NA
rckv[rckv < 0] <- NA
gvlv[gvlv < 0] <- NA
sndv[sndv < 0] <- NA
mudv[mudv < 0] <- NA

## Scale composition from 100% to 0-1
rckv <- rckv / max(values(rckv), na.rm=T)
gvlv <- gvlv / max(values(gvlv), na.rm=T)
sndv <- sndv / max(values(sndv), na.rm=T)
mudv <- mudv / max(values(mudv), na.rm=T)

## Figure
png(paste0(dir_fig, "Ecospace-seabed-types.png"), 
    width = 9, height = 6, units = "in", res=1200)
par(mfrow=c(2,2))
plot(rckv, colNA = "gray", main = "Rock"); plot(gvlv, colNA = "gray", main = "Gravel")
plot(sndv, colNA = "gray", main = "Sand");  plot(mudv, colNA = "gray", main = "Mud");  
dev.off()
par(mfrow=c(1,1))

## Write out ASCII files for Ecospace
if(!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE) ## Create output directory if needed

terra::writeRaster(rckv, file.path(dir_out, "sedcomp-rock.asc"),
                   filetype = "AAIGrid", overwrite = TRUE)

terra::writeRaster(gvlv, file.path(dir_out, "sedcomp-gravel.asc"),
                   filetype = "AAIGrid", overwrite = TRUE)

terra::writeRaster(sndv, file.path(dir_out, "sedcomp-sand.asc"),
                   filetype = "AAIGrid", overwrite = TRUE)

terra::writeRaster(mudv, file.path(dir_out, "sedcomp-mud.asc"),
                   filetype = "AAIGrid", overwrite = TRUE)
