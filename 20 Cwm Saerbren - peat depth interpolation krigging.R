## 20 Cwm Saerbren - peat depth interpolation/ krigging####

# This script take peat depth data and creates an interpolated peat depth layer e.

# Mike Shewring
# 09 06 2020

# Required Packages
# install.packages("spatialeco")
library(spatialEco)
library(raster)
library(rgdal)
library(gdalUtils)
library(rgrass7)
library(link2GI)
library(sf)
library(tidyverse)
library(gstat)
library(scales)
library(mapview)

pdepth<-read.csv(file.choose()) %>% st_as_sf(coords = c('x','y'), crs = 27700)

pdepth$Depth <-as.numeric(pdepth$Peat.Depth)

plot(pdepth)

hist(pdepth$Depth)

aoi<-st_read("E:\\lost peatlands\\shp-files\\Lost peatlands.shp")%>%st_transform(27700)
aoi
aoi$Type
plot(aoi[1])
aoi<-aoi[aoi$Site == "Cwm Saerbren"]
aoi<-st_buffer(aoi, 150)
viewExtent(aoi[aoi$Site == "Cwm Saerbren"])

viewExtent(pdepth)
pd<-as_Spatial(pdepth)

pdepth<-st_transform(pdepth, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")
pd<-as_Spatial(pdepth)

# https://mgimond.github.io/Spatial/interpolation-in-r.html#kriging
# Define the 1st order polynomial equation
f.1 <- as.formula(Depth ~ X + Y) 
# Add X and Y to P
pd$X <- coordinates(pd)[,1]
pd$Y <- coordinates(pd)[,2]

# Run the regression model
lm.1 <- lm( f.1, data=pd)

# # Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(pd, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(pd)

# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.1st)
#r.m <- mask(r, aoi)
plot(r)

library(tmap)
# Plot
tm_shape(r) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted peat depth \n(in m)") + 
  tm_shape(pd) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# krigging irregular points
# Define the 1st order polynomial equation
f.1 <- as.formula(Depth ~ X + Y) 

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, pd, cloud = FALSE)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = T, fit.sills = T,
                          vgm(psill=0.2, model="Sph", range=200, nugget=0.05))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,250), ylim=c(0,0.8))
# Define the trend model
f.1 <- as.formula(Depth ~ X + Y) 

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, pd, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
#r.m <- mask(r, aoi)

# Plot the map
tm_shape(r) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="peat Depth (m)") +
  tm_shape(pd) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# variance
r1   <- raster(dat.krg, layer="var1.var")
#r.m1 <- mask(r1, aoi)

tm_shape(r) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance peat depth map") +tm_shape(pd) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# 95% confiedence intervals
r2   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m2 <- mask(r2, aoi)

tm_shape(r) + 
  tm_raster(n=7, palette ="Reds",
            title="95% CI map peat depth (m)") +tm_shape(pd) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# export raster
writeRaster(r, "ClassroomPeatDepth_20200618.tif", overwrite = T)
writeRaster(r1, "ClassroomPeatDepthVariance_20200618.tif", overwrite = T)
writeRaster(r2, "ClassroomPeatDepth95%conf_20200618.tif", overwrite = T)

# krigging # original regular grid ####
pd<-as_Spatial(pdepth)
pd<-as(pd, "SpatialPixelsDataFrame")
pd<-as(pd, "SpatialPointsDataFrame")
pd
gs <- gstat(formula=Depth~1, locations=pd)
v <- variogram(gs, width=20)
head(v)
plot(v)

# fit variogram model
fve <- fit.variogram(v, vgm(c("Exp", "Sph", "Mat")), fit.kappa = seq(.8,10,.01))
fve
# check this out -https://gdmdata.com/media/documents/handouts/2017ASA_FarmToTable_Variogram.pdf?v=1507058794
#
#fve <- fit.variogram(v, vgm(500,"Sph",40,10), fit.kappa = seq(.3,5,.01))
#fve

plot(variogramLine(fve, 250), type='l', ylim=c(0,1))
points(v[,2:3], pch=20, col='red')

# fit gstat model with depth and fitted variogram
k <- gstat(formula=Depth~1, locations=pd, model=fve)

pdepth

# make a raster
r1<- raster (xmn = 292180, xmx=292670,  ymn= 202084,ymx= 202486, res=10)
projection(r1) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.4889999999569 +units=m +no_defs"
r1

g <- as(r1, 'SpatialGrid')
g
plot(g)
kp <- predict(k, g)
kp
## plot
spplot(kp)


# convert to raster
gridded(kp)
plot(kp)
foo<- kp %>% as.data.frame %>% 
  dplyr::select("s1", "s2","var1.pred")
coords<-foo[, c("s1", "s2")]
foo<-kp %>% as.data.frame %>% 
  dplyr::select("var1.pred")
crs1<-CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")
x<-SpatialPixelsDataFrame(points = coords, data = foo, proj4string = crs1 )
plot(x)

# convert to raster
par(mfrow=c(1,1))
rdepth<-raster(x)
plot(rdepth) 

# export raster
writeRaster(rdepth, "CreganPeatDepth_20200211.tif", overwrite = T)

# krigging from mineral #####
# load DSM
ras<-raster("E:\\lost peatlands\\NRW_lidar\\cregan\\mosaic\\cregan_0.5_lidar.tif")
ras


# plot - make sure they overlap sufficently
myCol = terrain.colors(12)
plot(ras,
     breaks = c(395, 405, 415, 425, 435, 445, 455, 465, 475, 485, 495, 505), 
     col = myCol,
     main= "Digital Surface Model", 
     xlab = "BNG Westing Coordinate (m)", 
     ylab = "BNG Northing Coordinate (m)")
plot(pdepth$geometry, add = T)
plot(pdepth[1],add=T)


# extract dsm values for peat sample points
# raster::extract
pdepth1<-raster::extract(ras,pdepth, df=T)

# add these to the peat depth data frame
pdepth$dsm<-pdepth1$cregan_0.5_lidar

# find mineral surface layer
pdepth$ml<-pdepth$dsm-pdepth$Depth
#write.csv(pdepth, "MM_BarePeatProbingResults2.csv")
hist(pdepth$ml)
hist(pdepth$Depth)
pdepth2<-pdepth%>%filter(!is.na(Depth))
pdepth2<-st_transform(pdepth2,crs = " +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy
+towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")

# krigging
pd<-as_Spatial(pdepth2)
pd<-as(pd, "SpatialPixelsDataFrame")
pd<-as(pd, "SpatialPointsDataFrame")

library(gstat)

gs <- gstat(formula=ml~1, locations=pd)
v <- variogram(gs, width=20)
head(v)
plot(v)

# fit variogram model
fve <- fit.variogram(v, vgm(c("Exp", "Sph", "Mat")), fit.kappa = seq(.3,5,.01))
fve
?fit.variogram
#model    psill    range kappa
#1   Nug  0.00000   0.0000   0.0
#2   Mat 35.98277 145.5248   1.2

plot(variogramLine(fve, 1000), type='l', ylim=c(0,50))
points(v[,2:3], pch=20, col='red')
pdepth2
k <- gstat(formula=ml~1, locations=pd, model=fve)

# make blank raster
ras
r1<- raster (xmn = 285337, xmx=286524.5,  ymn= 198626,ymx= 200582, res=0.5)
projection(r1) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs"
r1

g <- as(r1, 'SpatialGrid')
g
plot(g)
kp <- predict(k, g)

## plot
spplot(kp)


# convert to raster
gridded(kp)
plot(kp)
foo<- kp %>% as.data.frame %>% 
  dplyr::select("s1", "s2","var1.pred")
coords<-foo[, c("s1", "s2")]
foo<-kp %>% as.data.frame %>% 
  dplyr::select("var1.pred")
crs1<-CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")
x<-SpatialPixelsDataFrame(points = coords, data = foo, proj4string = crs1 )
plot(x)

# convert to raster
par(mfrow=c(1,1))
rdepth<-raster(x)
plot(rdepth) 

# export raster
writeRaster(rdepth, "CreganMineral_20200211.tif", overwrite = T)

pras<-ras-rdepth
plot(pras)
pras[pras <0]<-0
pras[pras >3]<-3
writeRaster(pras, "CreganPeatFromMineral_20200211.tif", overwrite = T)
