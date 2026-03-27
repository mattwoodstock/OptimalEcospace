rm(list=ls()); graphics.off(); windows()

library(sf)
library(ggplot2)
library(terra)
library(raster)

dir_in  = "./data/dbSeaBed"
dir_out = "./outputs-for-ewe/sediments/"
dir_fig = "./figures/"

depth = terra::rast("./inputs/NGoM/basemap.asc")
plot(depth)

##------------------------------------------------------------------------------
## Seabed sediment composition 
seabed  = sf::st_read(file.path(dir_in, "usSEABED_GOM_Sediments.shp"))

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

## Rasterize polygons
rckv <- terra::rasterize(seabed, depth, field = "gom_rckv")
gvlv <- terra::rasterize(seabed, depth, field = "gom_gvlv")
sndv <- terra::rasterize(seabed, depth, field = "gom_sndv")
mudv <- terra::rasterize(seabed, depth, field = "gom_mudv")

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
#png(paste0(dir_fig, "Ecospace-seabed-types.png"), 
#    width = 9, height = 6, units = "in", res=1200)
par(mfrow=c(2,2))
plot(rckv, colNA = "gray", main = "Rock"); plot(gvlv, colNA = "gray", main = "Gravel")
plot(sndv, colNA = "gray", main = "Sand");  plot(mudv, colNA = "gray", main = "Mud");  
dev.off()

## Write out ASCII files for ecospace
terra::writeRaster(rckv, paste0(dir_out, "/seabed-sedcomp-rock"),   format = 'ascii', overwrite=TRUE)
terra::writeRaster(gvlv, paste0(dir_out, "/seabed-sedcomp-gravel"), format = 'ascii', overwrite=TRUE)
terra::writeRaster(sndv, paste0(dir_out, "/seabed-sedcomp-sand"),   format = 'ascii', overwrite=TRUE)
terra::writeRaster(mudv, paste0(dir_out, "/seabed-sedcomp-mud"),    format = 'ascii', overwrite=TRUE)
