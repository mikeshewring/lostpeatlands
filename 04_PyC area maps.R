# 04_PyC area maps

# email dated 28/08/2019 Alistair Hinton sets out following objectives
# Use high-res’ LIDAR ground surface elevation data to map slope angle (0.5 deg increments) across the PyC HMP areas.  

# Derive slope v. areas statistics and graphics, and interpret.

# Propose slope envelope v. restoration treatment options, based on slope envelope areas and restoration treatment costs (liaise with Gareth Roberts)

# Packages ####
library(dplyr)
library(readr)
library(spatialEco)
library(raster)
library(rgdal)
library(gdalUtils)
library(rgrass7)
library(link2GI)
library(sf)
library(tidyverse)
library(mapview)

# 01. PyC load mask tiled raster and mosaic ##### better in python
list<-list.files("E:\\PyC LIDAR\\P_11353\\LIDAR\\slope\\mask", full.names=T)
list
list1<-list.files("E:\\PyC LIDAR\\P_11352\\LIDAR\\slope\\mask", full.names=T)
list1
list2<-c(list,list1)
list2

ListRasters <- function(list_names) {
  raster_list <- list() # initialise the list of rasters
  for (i in 1:(length(list_names))){ 
    grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
    raster_file <- raster(grd_name)
  }
  raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
}

list_names <- NULL
for (i in 1:length(list2)) {
  list_names <- c(list_names, list2[i])
}

raster.list <-sapply(list_names, FUN = ListRasters)
names(raster.list) <- NULL
raster.list$fun <- mean
mos <- do.call(mosaic, raster.list)
mos
writeRaster(mos, "PyCSlopeMos_20190927.tif")
plot(mos)

?aggregate
r<-raster::aggregate(mos, fact = 10, fun = mean, na.rm=T)
r
writeRaster(r, "PyCSlopeMos_20190927_agg10.tif")
hist(r)
hist(mos)
r[r>=6] <- NA
writeRaster(r, "PyCSlopeMos_20190927_agg106deg.tif")
r<-raster("PyCSlopeMos_20190927_agg10.tif")
r[r>=5] <- NA
writeRaster(r, "PyCSlopeMos_20190927_agg105deg.tif")
r<-raster("PyCSlopeMos_20190927_agg10.tif")
r[r>=4] <- NA
writeRaster(r, "PyCSlopeMos_20190927_agg104deg.tif")
r<-raster("PyCSlopeMos_20190927_agg10.tif")
r[r>=3] <- NA
writeRaster(r, "PyCSlopeMos_20190927_agg103deg.tif")
r<-raster("PyCSlopeMos_20190927_agg10.tif")
r[r>=2] <- NA
writeRaster(r, "PyCSlopeMos_20190927_agg102deg.tif")
r<-raster("PyCSlopeMos_20190927_agg10.tif")
r[r>=1] <- NA
writeRaster(r, "PyCSlopeMos_20190927_agg101deg.tif")
# Reproject and prep for leaflet #####
library(tidyverse)
crs1 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
r6<-raster("PyCSlopeMos_20190927_agg106deg.tif") %>% projectRaster(crs=crs1)
r5<-raster("PyCSlopeMos_20190927_agg105deg.tif") %>% projectRaster(crs=crs1)
r4<-raster("PyCSlopeMos_20190927_agg104deg.tif") %>% projectRaster(crs=crs1)
r3<-raster("PyCSlopeMos_20190927_agg103deg.tif") %>% projectRaster(crs=crs1)
r2<-raster("PyCSlopeMos_20190927_agg102deg.tif") %>% projectRaster(crs=crs1)
r1<-raster("PyCSlopeMos_20190927_agg101deg.tif") %>% projectRaster(crs=crs1)
## leaflet map of raster slopes for pyc
plot(r6)
# leaflet map of species records
library(leaflet)
?colorNumeric
pal <- colorNumeric(c("#00f7b5", "#1a02f0", "#666666"), values(r6),
                    na.color = "transparent")

map<-leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
  addRasterImage(r6, colors = pal) %>%
  addLegend(pal = pal, values = values(r6),
            title = "Slope")

library(htmlwidgets)
saveWidget(map, file="pycslope6degmap.html", selfcontained=FALSE)

pal <- colorNumeric(c("#00f7b5", "#1a02f0", "#666666"), values(r5),
                    na.color = "transparent")

leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
  addRasterImage(r5, colors = pal) %>%
  addLegend(pal = pal, values = values(r5),
            title = "Slope")

pal <- colorNumeric(c("#00f7b5", "#1a02f0", "#666666"), values(r4),
                    na.color = "transparent")

leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
  addRasterImage(r4, colors = pal) %>%
  addLegend(pal = pal, values = values(r4),
            title = "Slope")

pal <- colorNumeric(c("#00f7b5", "#1a02f0", "#666666"), values(r3),
                    na.color = "transparent")

leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
  addRasterImage(r3, colors = pal) %>%
  addLegend(pal = pal, values = values(r3),
            title = "Slope")

pal <- colorNumeric(c("#00f7b5", "#1a02f0", "#666666"), values(r2),
                    na.color = "transparent")

leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
  addRasterImage(r2, colors = pal) %>%
  addLegend(pal = pal, values = values(r2),
            title = "Slope")


library(htmlwidgets)
saveWidget(map, file="pycslope6degmap.html", selfcontained=FALSE)
