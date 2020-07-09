## 13 Cregan -  peat peat depth interpolation/ krigging####

# This script take peat depth data and creates an interpolated peat depth layer e.

# Mike Shewring
#11 02 2020

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

pdepth<-read.csv("E:\\lost peatlands\\Field survey\\CreganPD.csv") %>% st_as_sf(coords = c('x','y'), crs = 27700)

pdepth$Depth<-as.numeric(pdepth$PeatDepth)

plot(pdepth)

hist(pdepth$Depth)


# krigging
pd<-as_Spatial(pdepth)
pd<-as(pd, "SpatialPixelsDataFrame")
pd<-as(pd, "SpatialPointsDataFrame")

gs <- gstat(formula=Depth~1, locations=pd)
v <- variogram(gs, width=20)
head(v)
plot(v)

# fit variogram model
fve <- fit.variogram(v, vgm(c("Exp", "Sph", "Mat")), fit.kappa = seq(.3,5,.01))
fve
# check this out -https://gdmdata.com/media/documents/handouts/2017ASA_FarmToTable_Variogram.pdf?v=1507058794
#
#fve <- fit.variogram(v, vgm(500,"Sph",40,10), fit.kappa = seq(.3,5,.01))
#fve

plot(variogramLine(fve, 1000), type='l', ylim=c(0,0.3))
points(v[,2:3], pch=20, col='red')

# fit gstat model with depth and fitted variogram
k <- gstat(formula=Depth~1, locations=pd, model=fve)

pdepth

# make a raster
r1<- raster (xmn = 285250, xmx=286400,  ymn= 198700,ymx= 200500, res=10)
projection(r1) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs"
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
