#02_lost_peatlands_desk_study - GBIF

# Mike shewring 
# 24 09 19

# script to download GBIF datasets for HRA's and CWS

# packages ####
library(rgbif)
library(tidyverse)
library(spatialEco)
library(raster)
library(rgdal)
library(gdalUtils)
library(rgrass7)
library(link2GI)
library(sf)
library(tidyverse)
library(mapview)

# example tutorial https://poldham.github.io/abs/mapgbif.html

# spatial bounds
aoi<-st_read("E:\\lost peatlands\\shp-files\\Lost peatlands.shp")%>%st_transform(27700)
aoi
aoi$Type
plot(aoi[1])
viewExtent(aoi)
aoi84<-st_transform(aoi, 4326)

# down data from gbif
#https://www.gbif.org/occurrence/download/0003165-190918142434337
# 0003165-190918142434337

library(dplyr)
#aoi_gbif <- occ_download_get(key = "0003165-190918142434337",overwrite = TRUE)%>%  occ_download_import(aoi_gbif_download, na.strings = c("", NA)) not working....

# reading data in with readr drops 166000 rows
#aoi_gbif <- readr::read_delim("E:\\lost peatlands\\desk study\\protected species\\gbif\\0003165-190918142434337.csv", delim = "\t", escape_double = FALSE, 
#                                      col_names = TRUE, na = c("", "NA"))


# data table solution
library(data.table)
aoi_gbif <- fread("E:\\lost peatlands\\desk study\\protected species\\gbif\\0003165-190918142434337.csv", na.strings = c("", NA))
head(aoi_gbif)
tail(aoi_gbif)
library(dplyr)
aoi_gbif %>% drop_na(kingdom) %>% count(kingdom, sort = TRUE)
aoi_gbif %>% filter(taxonRank == "SPECIES") %>% count(species) %>% arrange(desc(n))


# plot of data by group - is dom by birds?
library(ggplot2)
aoi_gbif %>% count(class, sort = TRUE) %>% drop_na(class) %>% filter(n > 3000)%>% ggplot(aes(x = reorder(class, n), y = n, fill = class)) + geom_bar(stat = "identity", show.legend = FALSE) + labs(x = "Class of Organism", y = "Number of Occurrence Records (observations)") + coord_flip()

# remove birds to separate table
not_birds <- filter(aoi_gbif, class != "Aves")
aoi_gbif$decimalLatitude
aoi_gbif$decimalLongitude


aoi_gbif <- dplyr::rename(aoi_gbif , latitude = decimalLatitude, 
                                  longitude = decimalLongitude)

aoi_slim <- aoi_gbif[, .(gbifID, kingdom, class, order, species, latitude, longitude, 
                         locality, day, month, year)] 

foo<-st_as_sf(aoi_slim, coords=c(7:6), crs=4236)%>%st_transform(27700)
#viewExtent(foo)

foo1<-st_crop(foo, aoi)
foo1<-st_transform(foo1,4236)
coords<-st_coordinates(foo1)
foo2<-cbind(foo1,coords)
summary(as.factor(foo2$kingdom))
summary(as.factor(foo2$class))
st_write(foo2, "LP_GBIFdata_2008to20019.shp")

class<-c("Actinopterygii","Magnoliopsida", "Amphibia", "Andreaeopsida", "Aves", "Bryopsida", "Insecta", "Jungermanniopsida", "Lecanoromycetes", "Liliopsida", "Mammalia", "Marchantiopsida",  "Sphagnopsida")

# leaflet map of species records
library(leaflet)
library(RColorBrewer)
previewColors(colorFactor("Paired", domain = NULL), LETTERS[1:14])
my_palette <- c("#e6194B","#3cb44b","#ffe119","#4363d8",
               "#f58231","#911eb4","#42d4f4", "#f032e6",
               "#bfef45","#000000","#469990","#e6beff","#9A6324")

previewColors(colorFactor(my_palette, levels = class), class)
factpal <- colorFactor(my_palette, levels = class)

species_circle <- leaflet(foo2) %>% addTiles() %>% 
  addCircleMarkers(~X, ~Y, popup = foo2$species, radius = 10, 
                   weight = 2, opacity = 0.5, fill = TRUE, 
                   fillOpacity = 0.2, color = ~factpal(class))
species_circle

# with legend
groups = unique(foo2$class)
?addCircleMarkers
map = leaflet(foo2) %>% addTiles(group = "OpenStreetMap")
for (i in groups) {
  data = foo2[foo2$class == i, ]
  map = map %>% addCircleMarkers(data = data, ~X, ~Y, popup = data$species,
                                 radius = 5, weight = 2, opacity = 0.5, fill = TRUE, 
                                 fillOpacity = 0.2, color = ~factpal(class), 
                                 group = i,clusterOptions = 
                                   markerClusterOptions()) %>% 
    addPolygons(data=aoi84, popup = aoi84$Site, 
                fillColor = "#3cb44b", 
                fillOpacity = 0.1, opacity = 0.05)
}
map<-map %>% addLayersControl(overlayGroups = groups, options = layersControlOptions(collapsed = FALSE))
map

library(htmlwidgets)
saveWidget(map, file="LostPeatlandsGBIFDatamap2.html", selfcontained=FALSE)

