## 11_Pyc Peat modelling

# This script take peat depth data and uses this to create a interpolated mineral layer elevation surface using krigging. This surface is then used to subtract from the DSM to give the the approximate peat depths across the area of interest (AOI).

# Mike Shewring
# 16 12 2019

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
pdepth<-read.csv("E:\\PyC\\peat-modelling19_20\\PeatDatComb19CE.csv")%>%st_as_sf(coords = c('x','y'), crs = 27700)
plot(pdepth[1])

hist(pdepth$depth_m)
summary(pdepth$depth_m)
foo<-pdepth [pdepth$depth_m < 0.5,]
summary(pdepth$year)

pdepth<-pdepth%>%filter(!year == 2012)
summary(pdepth$year)
hist(pdepth$depth_m)
foo<-pdepth [pdepth$depth_m < 0.5,]
248/469*100
### load elevation and slope data and use these as co_variates in krigging?
# load DSM
ras<-raster("E:\\PyC LIDAR\\P_11352_11353_smooth\\dtm_cropped.tif")
ras
plot(ras)
ras<-ras/100
ras


# plot - make sure they overlap sufficently
myCol = terrain.colors(10)
plot(ras,
     breaks = c(200, 250, 300, 350, 400, 450,500, 550, 600,650), 
     col = myCol,
     main="Digital Surface Model", 
     xlab = "BNG Westing Coordinate (m)", 
     ylab = "BNG Northing Coordinate (m)")
plot(pdepth$geometry, add = T)
plot(pdepth[1],add=T)


# extract dsm values for peat sample points
# raster::extract
pdepth1<-raster::extract(ras,pdepth, df=T)
pdepth1

pdepth$dtm<-pdepth1[,2]
foo<-pdepth[pdepth$dtm <0,]
foo$dtm<-NA
foo
foo1<-pdepth[!pdepth$dtm <0,]
foo<-rbind(foo,foo1)
pdepth=foo
rm(foo,foo1)

# elevation relationship

plot(pdepth$depth_m~pdepth$dtm)

# slope
# load slope
ras<-raster("E:\\PyC LIDAR\\P_11352_11353_smooth\\s3\\s3_PyCSlopeMos_20191003.tif")
ras
plot(ras)

# slope extract
pdepth1<-raster::extract(ras,pdepth, df=T)
pdepth1
pdepth$slope<-pdepth1[,2]
rm(pdepth1)

# find mineral layer
# find mineral surface layer
pdepth$ml<-pdepth$dtm-pdepth$depth_m

# export data
write.csv(pdepth, "E:\\PyC\\peat-modelling19_20\\PyC_Peat1519datDatComb20191216.csv")

hist(pdepth$ml)
summary(pdepth$ml)
hist(pdepth$depth_m)
pdepth2<-pdepth%>%filter(!is.na(ml))

# transform to same crs
pdepth2<-st_transform(pdepth2,crs = " +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy
+towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")

# krigging
pd<-as_Spatial(pdepth2)

gs <- gstat(formula=ml~1, locations=pd)
v <- variogram(gs, width=20)
head(v)

plot(v)

# fit variogram model
fve <- fit.variogram(v, vgm(c("Exp", "Sph", "Mat")), fit.kappa = seq(.3,5,.01))
fve <- fit.variogram(v, vgm(c("Exp", "Sph")),fit.sills = TRUE, fit.ranges = TRUE,
                            fit.method = 7, debug.level = 1, fit.kappa = T)

?fit.variogram

fve


plot(variogramLine(fve, 5000), type='l', ylim=c(0,5000))
points(v[,2:3], pch=20, col='red')
pdepth2

k <- gstat(formula=ml~1, locations=pd, model=fve)


# make blank raster
extent(pdepth)
r1<- raster (xmn = 289417, xmx=291883,  ymn= 198262,ymx= 203302, res=10)
projection(r1) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs"
r1

g <- as(r1, 'SpatialGrid')
g
plot(g)
kp <- predict(k, g)

## plot
spplot(kp)

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

ras

# resample to same extent and resolution as aoi DSM
# load dsm
ras<-raster("E:\\PyC LIDAR\\P_11352_11353_smooth\\dtm_cropped.tif")
ras

# crop to aoi spatial extent
ras<-crop(ras,rdepth)
plot(ras)
ras<-ras/1000
# resample to same res 
mineral<-resample(rdepth, ras, method="bilinear")
mineral
plot(mineral)

# check the extent and res are the same
res(mineral)==res(ras)

extent(mineral)==extent(ras)

# subtract mineral surface from smoothed dsm to give peath depth
PyCpeat<-ras-mineral
plot(PyCpeat) # some <0 values in here this is not correct so set these to 0
PyCpeat[PyCpeat < 0]<-0
#PyCpeat[PyCpeat >4]<-0 # some >4 values in here this is not correct so set these to 0
plot(PyCpeat)

# export raster
writeRaster(PyCpeat, "PyCpeathfromMin20191217.tif",overwrite=TRUE)
PyCpeat<-raster("PyCpeathfromMin20191217.tif")
PyCpeat[PyCpeat >10]<-0 # some >4 values in here this is not correct so set these to 0
PyCpeat[PyCpeat >4]<-4 
plot(PyCpeat)
writeRaster(PyCpeat, "PyCpeathfromMin20191217RevA.tif",overwrite=TRUE)
# crop to HMA
shp<-st_read("E:\\PyC\\HMP_Redraft2.shp")
PyCpeat<-crop(PyCpeat,shp)
PyCpeat<-mask(PyCpeat,shp)
plot(PyCpeat)
writeRaster(PyCpeat, "PyCpeathfromMin20191217RevB.tif",overwrite=TRUE)
# simple krigging of peath depth data ####
pd
k <- gstat(formula=depth_m~1, locations=pd, model=fve)
k
# make blank raster
r1<- raster (xmn = 289417, xmx=291883,  ymn= 198262,ymx= 203302, res=10)
projection(r1) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs"
r1

g <- as(r1, 'SpatialGrid')
g
plot(g)


## plot
spplot(kp)

# predicted values
kp <- predict(k, g)


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
rdepth[rdepth < 0]<-0

# crop to HMA
shp<-st_read("E:\\PyC\\HMP_Redraft2.shp")
rdepth1<-crop(rdepth,shp)
rdepth1<-mask(rdepth1,shp)
plot(rdepth1)

# export raster
writeRaster(rdepth, "PyCSimpKrig_20191217.tif", overwrite = T)
writeRaster(rdepth1, "PyCSimpKrig_20191217_RevA.tif", overwrite = T)

