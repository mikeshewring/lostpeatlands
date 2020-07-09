# 12_PyC_Peat_modelling_and_mapping

# Script to filter peat depths by depth and polyginise

# Mike Shewwring

# 22 1 2020

#librarys
library(raster)
library(sf)
library(tidyverse)
library(ggplot2)

# import raster
rdepth<- raster("PyCSimpKrig_20191217_RevA.tif")
rdepth1<-raster("PyCpeathfromMin20191217RevB.tif")

#~~~~~~~~~ Step 1. Mask raster with FCW soils data so we only have peat where the datasets agree
# import shapefile for mask
shp<-st_read(file.choose())%>%st_transform("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")

# mask
rdepth<-raster::mask(rdepth, shp)
plot(rdepth)

rdepth1<-raster::mask(rdepth1, shp)
plot(rdepth1)

# create ground smoothing areas
rdepthGS<-calc(rdepth, function (x){ x[x< 1] <-NA; return(x)})
plot(rdepthGS)

rdepthGS1<-calc(rdepth1, function (x){ x[x< 1] <-NA; return(x)})
plot(rdepthGS1)

# cross tracking treatment areas
rdepthCT<-calc(rdepth, function (x){ x[x> 1] <-NA; return(x)})
rdepthCT<-calc(rdepthCT, function (x){ x[x< 0.5] <-NA; return(x)})
plot(rdepthCT)

rdepthCT1<-calc(rdepth1, function (x){ x[x> 1] <-NA; return(x)})
rdepthCT1<-calc(rdepthCT1, function (x){ x[x< 0.5] <-NA; return(x)})
plot(rdepthCT1)

# polygonise
GSpolys<-rasterToPolygons(rdepthGS, n=16, na.rm=TRUE, digits=12, dissolve=FALSE)
plot(GSpolys)
GSpolys<-st_as_sf(GSpolys)
st_write(GSpolys, "GSpolys.shp")

GSpolys<-rasterToPolygons(rdepthGS1, n=16, na.rm=TRUE, digits=12, dissolve=FALSE)
plot(GSpolys1)
GSpolys1<-st_as_sf(GSpolys1)
st_write(GSpolys1, "GSpolys1.shp")
