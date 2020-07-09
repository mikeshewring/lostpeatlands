## 09 Castell Nos -  peat peat depth interpolation/ krigging from mineral####

# This script take peat depth data and uses this to create a interpolated mineral layer elevation surface using krigging. This surface is then used to subtract from the DSM to give the the approximate peat depths across the area of interest (AOI).

# Mike Shewring
# 03 09 2019

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

#### load peat data
pdepth<-read.csv("E:\\lost peatlands\\Field survey\\CNPeatPoints20191106.csv")%>%st_as_sf(coords = c('x','y'), crs = 27700)
pdepth$Depth<-as.numeric(pdepth$Depth)

pdepth1<-read.csv("E:\\lost peatlands\\RLostPeat\\CastellNos20191106.csv")%>%st_as_sf(coords = c('x','y'), crs = 27700)

pdepth2<-read.csv("E:\\lost peatlands\\RLostPeat\\RigareCastellNos_GIS.csv")%>%st_as_sf(coords =c("x","y"), crs = 27700)

pdepth1$Depth<-as.numeric(pdepth1$Depth)
pdepth2$Depth<-as.numeric(pdepth2$Depth)


pdepth2<-pdepth2%>%select(Depth)
pdepth1<-pdepth1%>%select(Depth)
pdepth<-pdepth%>%select(Depth)
pdepth<-rbind(pdepth, pdepth1, pdepth2)
hist(pdepth$Depth)
rm(pdepth1, pdepth2)

plot(pdepth[1])
pdepth<-pdepth%>%filter(!is.na(pdepth$Depth))
st_write(pdepth, "mergepdepth1.shp")

# load DSM
ras<-raster("E:\\lost peatlands\\NRW_lidar\\castellnos\\mosaic\\castellnos_0.5_lidar1.tif")
ras


# plot - make sure they overlap sufficently
myCol = terrain.colors(10)
plot(ras,
     breaks = c(355, 365, 375, 385, 395, 405, 415,425,435,445), 
     col = myCol,
     main="Digital Surface Model\nMynydd Maen Bare Peat Site", 
     xlab = "BNG Westing Coordinate (m)", 
     ylab = "BNG Northing Coordinate (m)")
plot(pdepth$geometry, add = T)
plot(pdepth[1],add=T)


# extract dsm values for peat sample points
# raster::extract
pdepth1<-raster::extract(ras,pdepth, df=T)

# add these to the peat depth data frame
pdepth$dsm<-pdepth1$castellnos_0.5_lidar1

# find mineral surface layer
pdepth$ml<-pdepth$dsm-pdepth$Depth
#write.csv(pdepth, "MM_BarePeatProbingResults2.csv")
hist(pdepth$ml)
hist(pdepth$Depth)
pdepth2<-pdepth%>%filter(!is.na(ml))
pdepth2<-st_transform(pdepth2,crs = " +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy
+towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")

# krigging
pd<-as_Spatial(pdepth2)
#pd<-as(pd, "SpatialPixelsDataFrame")
#pd<-as(pd, "SpatialPointsDataFrame")

library(gstat)

gs <- gstat(formula=ml~1, locations=pd)
v <- variogram(gs, width=20)
head(v)
plot(v)

# fit variogram model
fve <- fit.variogram(v, vgm(c("Exp", "Sph", "Mat")), fit.kappa = seq(.3,5,.01))
fve
?fit.variogram

##model   psill    range
##1   Nug 0.00000  0.00000
##2   Exp 0.66185 54.44908

plot(variogramLine(fve, 400), type='l', ylim=c(0,450))
points(v[,2:3], pch=20, col='red')
pdepth2
k <- gstat(formula=ml~1, locations=pd, model=fve)

# make blank raster
r1<- raster (xmn = 296300, xmx=297600,  ymn= 199885,ymx= 201285, res=10)
projection(r1) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs"
r1

g <- as(r1, 'SpatialGrid')
g
plot(g)
kp <- predict(k, g)

## plot
spplot(kp)

# predicted values
kp <- predict(k, g)

plot
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

## reload raster and plot
r<-raster("testkrig.tif")
par(mfrow=c(1,1))
plot(r) 
ras
# resample to same extent and resolution as aoi DSM
mineral<-resample(rdepth, ras, method="bilinear")
mineral
plot(mineral)

# check the extent and res are the same
res(mineral)==res(ras)

extent(mineral)==extent(ras)

# subtract mineral surface from smoothed dsm to give peath depth
MM_AEBpeat<-ras-mineral
plot(MM_AEBpeat) # some <0 values in here this is not correct so set these to 0
MM_AEBpeat[MM_AEBpeat < 0]<-0
MM_AEBpeat[MM_AEBpeat >4]<-0
plot(MM_AEBpeat)

# export raster
writeRaster(MM_AEBpeat, "CastellNosPeatDepthfromMin20191113.tif",overwrite=TRUE)

# plot for report
myCol <- terrain.colors(9, alpha = 0.75)
plot(MM_AEBpeat,
     breaks = c(0.0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5), 
     col = myCol,
     main="\nCastell Nos Peat Depth Map", 
     sub = "Calculated from DSM and Peat Probe Data",
     axes=FALSE,
     box = FALSE)

