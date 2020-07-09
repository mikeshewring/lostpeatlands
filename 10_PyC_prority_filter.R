install.packages("spatialEco")

library(spatialEco)
library(raster)
library(rgdal)
library(gdalUtils)
library(rgrass7)
library(link2GI)
library(sf)
library(tidyverse)
library(nngeo)

# hand redrawn areas ####
pyc<-st_read(file.choose())
head(pyc)
pyc$id=NULL
sum(pyc$area)/10000 # = 580.0512 ha
plot(pyc[2])
hist(pyc$area)
pyc<-pyc%>%filter(area >20000)
st_write(pyc, "E:\\PyC\\restoration area mapping\\PyC3to4deg2ha_20191216.shp")

nn<-st_nn(pyc, pyc, k=2, returnDist = T)
plot(pyc)

foo<-nn[[2]]
foo<-data.frame(foo)
foo<-foo[2,]
foo<-as.vector(unlist(foo))
pyc$nn<-foo

head(pyc)
plot(pyc[3])

