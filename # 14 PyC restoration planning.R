# 14 PyC restoration planning

library(sf)
library(raster)
library(tidyverse)

gs<-st_read(file.choose())
plot(gs)
foo<-st_cast(gs,"POLYGON")
foo
st_write(foo, "GroundSmothingAreas_PD1m+RevB.shp")

ct<-st_read(file.choose())
foo1<-st_cast(ct,"POLYGON")
foo1
plot(foo1)
st_write(foo1, "CrossTrackingAreas_PD_0.5_1mRevB.shp")
