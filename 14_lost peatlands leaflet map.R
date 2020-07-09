# Lost peatlands interactive maps #
# Mike shewring #
# 06 2020 #

#install.packages("leafpop")
#library ####
library(ggplot2)
library(tidyverse)
library(stringr)
library(sf)
library(mapview)
library(leaflet)
library(leafpop)
# spatial bounds ####
aoi<-st_read("E:\\lost peatlands\\shp-files\\Lost peatlands.shp")%>%st_transform(27700)
aoi
aoi$Type
plot(aoi[1])
viewExtent(aoi)
aoi84<-st_transform(aoi, 4326)
# lets read in field ustvey data
getwd()
l<-list.files("E:/lost peatlands/species records", pattern = ".csv", full.names = T)

dat<-lapply(l, read.csv)%>%bind_rows()
dat<-filter(dat,!is.na(dat$x))


dat<-st_as_sf(dat, coords = c("x","y"),crs=27700)
plot(dat[6])

mapview(dat)
dat$Sp<-dat$Binomial
dat<-separate(dat, Binomial, into = c("genus","sp"), sep = " " )
rm(dat1, foo)

dat<-dat%>%st_transform(4326)
coords<-st_coordinates(dat)
dat1<-cbind(dat,coords)
dat1<-dat1%>%filter(!is.na(dat$genus))
dat1<-dat1%>%filter(!dat$genus == "")
summary(as.factor(dat1$genus))
dat1<-st_drop_geometry(dat1)
unique(dat$genus)
# birds
dat2<-dat1%>%filter(genus == "Parus" | genus == "Carduelis" | genus == "Loxia" | genus == "Alauda" |genus == "Circus"| genus == "Anthus" | genus == "Falco"|genus=="Scolopax")%>%mutate(group="Birds")

# mammal
dat3<-dat1%>%filter(genus == "Dama" | genus == "Meles" | genus == "Microtus" | genus == "Arvicola" )%>%mutate(group="Mammals")

# inverts
dat4<-dat1%>%filter(genus =="aeshna" | genus =="Nemastomatidae" | genus =="Sphaeriusidae" | genus =="Aphaenogaster" |  genus =="Arion" | genus =="Batenus" | genus =="Adelosia" | genus =="Carabus" | genus =="Lycosidae" | genus =="Notiophilus" | genus =="Oligolophinae" | genus == "Oligolophinidae" | genus =="Pseudochorthippus" | genus =="Psilochorus" | genus =="Stygobromus" | genus =="Theridiidae" | genus =="Aglais")%>%mutate(group="Inverterbrates")

# amphibians and Reptiles
dat5<-dat1%>%filter(genus == "Bufo"|genus== "Rana"| genus=="Natrix")%>%mutate(group="Amphibians & Reptiles")
# plants

# Fungi
dat6<-dat1%>%filter(genus == "Hygrocybes"| genus== "Hygrophoropsis")%>%mutate(group="Funghi")

#lichen
dat7<-dat1%>%filter(genus =="Cladonia")%>%mutate(group="Lichen")

# plants
dat8<-dat1%>%filter(genus =="Pteridium" |genus =="Impatiens" |genus =="Dropteris" |genus =="Chamaenerion"| genus =="Aster"| genus =="Veronica"| genus =="Ranunculus"|genus =="Plantago"|genus =="Salix" |genus =="Pinus" |genus =="Nardus" |genus =="Blechnum" |genus =="Taraxacum" |genus =="Cirsium" |genus =="Thymus" |genus =="Prunella" |genus =="Narthecium" |genus =="Trichophorum" |genus =="Holcus" |genus =="Erica" |genus =="Eriophorum"| genus =="Digitalis" |genus =="Agrostis" |genus =="Crocosmia" |genus=="Festuca"| genus =="Molinia"| genus =="Epilobium"|genus =="Galium" |genus =="Dryopteris"|genus =="rubus" |genus =="Vaccinium" |genus =="Calluna"| genus =="Deschampsia" |genus =="Rumex" |genus =="Rhodedendron"|genus =="Drosera" |genus =="Picea" |genus =="Juncus"|genus =="Digitalis"| genus =="Carex" |genus =="Larix"| genus == "poa" | genus =="Luzula"| genus == "Betula"|genus == "Plantago"|genus =="Trifolium"| genus == "Helminthotheca"| genus == "Tussilago"|genus == "Veronica")%>%mutate(group="plants")

# Bryophytes
dat10<-dat1%>%filter(genus =="Pelia"|genus =="Athyrium"|genus =="Aulacomnium" |genus =="Calypogeia"|genus =="Cephalozia"|genus =="Dicranella" |genus =="Dicranum" |genus =="Hylocomium"|genus =="Hypnum"|genus =="Kindbergia" |genus =="Mnium" |genus =="Plagiothecium"|genus == "Pleurozium" |genus =="Polytrichum"|genus == "Pseudotaxiphyllum"|genus == "Rhytidiadelphus" |genus =="Rosella" |genus =="Thuidium"|genus == "Sympetrum"|genus == "Lophozia"|genus == "Campylopus" |genus =="Cephaloziella" |genus =="Pohlia"|genus == "Kurzia" |genus =="Calliergonella" |genus =="Diplophyllum" |genus =="Mniaecea"|genus == "Pogonatum"|genus == "Lymnocryptes" |genus =="Solenostoma" |genus =="Gymnocolea" |genus =="Hypochaeris" |genus =="Micarea" |genus =="Pilosella" |genus =="Porpidia"|genus == "Racomitrium" |genus =="Barbula" |genus == "Bryum"|genus == "Ceratodon" |genus =="Cratoneuron" |genus =="Dichodontium" |genus =="Didymodon" |genus =="Polytrichastrum" |genus =="Trichostomum"|genus == "Rytidiadelphus" |genus == "Dicranodontium"  |genus =="Monomorium"|genus == "Hylocomnium" |genus =="Lepidozia"|genus == "Lophocolea"|genus =="Mycena"|genus =="Plagiochila"|genus =="Pseudoscleropodium"|genus == "Sanionia" |genus =="Scapania" |genus =="Tetraphis"|genus =="Leucobryum"|genus == "Lophocolia" |genus =="Diplophylum"|genus =="Plagiomnium"|genus =="Huperzia"|genus =="Colura"|genus =="Peltigera"|genus =="Lophosia"|genus =="Lycosidae" |genus =="Monomortum")%>%mutate(group = "Bryophytes")


# Sphagnum
dat9<-dat1%>%filter(genus =="Sphagnum"|genus == 'Spahgnum')%>%mutate(group="Sphagnum")

foo<-bind_rows(dat2,dat3, dat4,dat5,dat6,dat7,dat8,dat9,dat10)
foo$Sp

group<-unique(foo$group)
summary(as.factor(foo$group))

# leaflet map of species records
library(leaflet)
library(RColorBrewer)
previewColors(colorFactor("Paired", domain = NULL), LETTERS[1:9])
my_palette <- c("#e6194B","#3cb44b","#ffe119","#4363d8",
                "#f58231","#911eb4","#42d4f4", "#f032e6",
                "#bfef45")

previewColors(colorFactor(my_palette, levels = group), group)
factpal <- colorFactor(my_palette, levels = group)

species_circle <- leaflet(foo) %>% addTiles() %>% 
  addCircleMarkers(~X, ~Y, popup = foo$Sp, radius = 10, 
                   weight = 2, opacity = 0.5, fill = TRUE, 
                   fillOpacity = 0.2, color = ~factpal(group))
species_circle
summary(is.na(foo$group))

summary(foo$group == "Birds")

# Map with legend ####
groups = unique(foo$group)
?addCircleMarkers
map = leaflet(foo) %>% addTiles()
for (i in groups) {
  data = foo[foo$group == i, ]
  map = map %>% addCircleMarkers(data = data, ~X, ~Y, popup = data$Sp,
                                 radius = 5, weight = 2, opacity = 0.5, fill = TRUE, 
                                 fillOpacity = 0.2, color = ~factpal(group), 
                                 group = i,clusterOptions = 
                                   markerClusterOptions()) %>% 
    addPolygons(data=aoi84, popup = aoi84$Site, 
                fillColor = "#3cb44b", 
                fillOpacity = 0.1, opacity = 0.05)
}
map<-map %>% addLayersControl(overlayGroups = groups, 
                              options = layersControlOptions(collapsed = FALSE))
map

library(htmlwidgets)
saveWidget(map, file="LostPeatlandsGBIFDatamap3.html", selfcontained=FALSE)

# heritage map ####
# spatial bounds ####
aoi<-st_read("E:\\lost peatlands\\shp-files\\Lost peatlands.shp")%>%st_transform(27700)
aoi
aoi$Type
plot(aoi[1])
viewExtent(aoi)
aoi84<-st_transform(aoi, 4326)

# load in data
dat<-read.csv(file.choose())
dat$x<-dat$Coordinates.X
dat$y<-dat$Coordinates.Y
dat<-filter(dat,!is.na(dat$x))
dat<-st_as_sf(dat, coords = c("x","y"),crs=27700)%>%st_transform(4326)
dat$x1<-st_coordinates(dat)[,1]
dat$y1<-st_coordinates(dat)[,2]

library(leaflet)
library(RColorBrewer)
previewColors(colorFactor("Paired", domain = NULL), LETTERS[1:9])
my_palette <- c("#e6194B","#3cb44b","#ffe119","#4363d8",
                "#f58231","#911eb4","#42d4f4", "#f032e6",
                "#bfef45","#2E4053","#6C3483","#27AE60","#FAD7A0",
                "#B3B6B7","#EBEDEF","#17202A")

group<-unique(dat$Period)
summary(as.factor(group))

?previewColors
previewColors(colorFactor(my_palette, levels = group), group)
factpal <- colorFactor(my_palette, levels = group)
dat1 = dat[!dat$Period == "Various", ]
group
map = leaflet(dat) %>% addTiles()
for (i in group) {
  data = dat1[dat1$Period == i, ]
  map = map %>% addCircleMarkers(data = data, ~x1, ~y1, 
                                 popup = paste("Name", dat2$Site.Name, "<br>",
                                               "Description:", dat2$Description, "<br>",
                                               "Designation:", dat2$Designation, "<br>",
                                               "Condition", dat2$Condition..Management, "<br>",
                                               "Ownership", dat2$Ownership, "<br>",
                                               "Threats", dat2$Threats, "<br>",
                                               "Opportunities",dat2$Opportunity.for.the.Project, "<br>",
                                               "Work proposed", dat$Project.Management.Work.Proposed, "<br>",
                                               "Programme Ref", dat$Management.Programme.Reference, "<br>",
                                               "Further Info.", dat$Links.to.Futher.Project.Information.plans),
                                 radius = 5, weight = 2, opacity = 0.5, fill = TRUE, 
                                 fillOpacity = 0.2, color = ~factpal(group), 
                                 group = i,clusterOptions = 
                                   markerClusterOptions()) %>% 
    addPolygons(data=aoi84, popup = aoi84$Site, 
                fillColor = "#3cb44b", 
                fillOpacity = 0.1, opacity = 0.05)
}
map<-map %>% addLayersControl(overlayGroups = group, 
                              options = layersControlOptions(collapsed = FALSE))
map

dat2 = dat[dat$Period == "Various", ]
map<-map%>%addCircleMarkers(data = dat2, ~x1, ~y1, 
                            popup = paste("Name", dat2$Site.Name, "<br>",
                                          "Description:", dat2$Description, "<br>",
                                          "Designation:", dat2$Designation, "<br>",
                                          "Condition", dat2$Condition..Management, "<br>",
                                          "Ownership", dat2$Ownership, "<br>",
                                          "Threats", dat2$Threats, "<br>",
                                          "Opportunities",dat2$Opportunity.for.the.Project, "<br>",
                                          "Work proposed", dat$Project.Management.Work.Proposed, "<br>",
                                          "Programme Ref", dat$Management.Programme.Reference, "<br>",
                                          "Further Info.", dat$Links.to.Futher.Project.Information.plans),
                            radius = 10, weight = 2, opacity = 0.5, fill = TRUE, 
                            fillOpacity = 0.9, color = "#e6194B", 
                            group = "Various",clusterOptions = 
                              markerClusterOptions())
map

library(htmlwidgets)
saveWidget(map, file="LostPeatlands_naturalheritage_map1_20200601.html", selfcontained=FALSE)

addPopupImages()


# images map ####
# load in data
# sites
aoi<-st_read("E:\\lost peatlands\\shp-files\\Lost peatlands.shp")%>%st_transform(27700)
aoi
aoi$Type
plot(aoi[1])
viewExtent(aoi)
aoi84<-st_transform(aoi, 4326)

# pyc
hmp<-st_read("C:\\Users\\C1751157\\Documents\\Lost peatlands\\PyC_CE_Boundary_MS_20200615.shp")%>%st_transform(4236)
plot(hmp)

# trials site
ts<-st_read('C:\\Users\\C1751157\\Documents\\Lost peatlands\\images//PyCTrialSite.shp')

#images
dat<-read.csv('C:\\Users\\C1751157\\Documents\\Lost peatlands\\photos1.csv')
dat<-st_as_sf(dat, coords = c("x","y"),crs=27700)%>%st_transform(4326)
dat$x1<-st_coordinates(dat)[,1]
dat$y1<-st_coordinates(dat)[,2]

library(leaflet)
library(RColorBrewer)

img<-as.character(dat$drivelink4)
?addTiles

# https://google-drive-direct-link.blogspot.com/ # website that converts google drive lonks to download links -  hack around fro google drive images
pnt<-dat%>%filter(dat$Location == "Pen y Cymoedd Trial Site")
map3 <- leaflet() %>%
  # Add a base layer
  addProviderTiles(providers$Esri.WorldImagery)%>%
  #addTiles() %>%
  # add markers
  addAwesomeMarkers(data= dat,lng=dat$x1, lat=dat$y1,
                    # add popup with images and variables as text
                    popup=~paste0("<style> div.leaflet-popup-content {width:auto !important;}</style>",
                                  "<img src='",img,"' width='640px' height='480px'>",
                                  "<br>",paste("Location:-",dat$Location, "<br>",
                                                     "Description: ",dat$Image.Ref,"<br>")),
                    popupOptions (maxWidth = "auto", minHeight ="auto"))%>%
  addPolygons(data=aoi84, popup = aoi84$Site, 
              fillColor = "#3cb44b", 
              fillOpacity = 0.1, opacity = 0.5, label = as.character(aoi84$Site))%>%
  addPolygons(data = hmp, fillColor = "#FFD700", color = "red",
              fillOpacity = 0.1, opacity = 0.5, label = "PyC HMA")%>%
  addPolygons(data = ts, fillColor = "#FF8C00", stroke = F,
              fillOpacity = 0.5, opacity = 0.5, label = "PyC Trial site")%>%
  addAwesomeMarkers(pnt,lng=pnt$x1, lat=pnt$y1, label = "PyC Restoration Works Video",
                    popup = leafpop:::popupIframe(as.character(pnt$drivelink4),width = 300, height = 225))
map3

?addPolygons
?addAwesomeMarkers
?popupOptions
?popupImage
?addPolygons
dat$Image.Ref

library(htmlwidgets)
saveWidget(map3, file="test_map3_20200608.html", selfcontained=FALSE)
?popupImage


# Natural biological heritage map ####
# spatial bounds ####
aoi<-st_read("C:\\Users\\C1751157\\Documents\\Lost peatlands\\2020_boundaries\\Proposed HRA Areas.shp")%>%st_transform(27700)
aoi
aoi$Type
plot(aoi[1])
viewExtent(aoi)
aoi84<-st_transform(aoi, 4326)

# load in data
list.files("C:\\Users\\C1751157\\Documents\\Lost peatlands\\", full.names = T, pattern =".shp")
df<-read.csv("C:\\Users\\C1751157\\Documents\\Lost peatlands\\Heritage Register - natural heritage bio.csv")
group<-unique(df$Designation)
summary(as.factor(group))

previewColors(colorFactor(my_palette, levels = group), group)
factpal <- colorFactor(my_palette, levels = group)
group

dat<-read.csv("C:\\Users\\C1751157\\Documents\\Lost peatlands\\Heritage Register - natural heritage bio pts.csv")
dat$wkt<-as.character(dat$wkt)
dat1<-dat[!dat$wkt == "",]
dat1$Site.Name
dat1$wkt<-as.character(dat1$wkt)
dat1<-st_as_sf(dat1, wkt= "wkt",crs=27700)%>% st_transform(4326) # corrupt geom 7, 17, 18, 27, 28, 30
plot(dat1)

dat2<-read.csv("C:\\Users\\C1751157\\Documents\\Lost peatlands\\Heritage Register - natural heritage bio polys.csv")
dat2$wkt<-as.character(dat2$wkt)
dat2<-dat2[!dat2$wkt == "",]
dat2$Site.Name
dat2<-dat2[!dat2$Site.Name == "Pen y Cymoedd Habitat Mangement Area (West)",]
dat2$wkt<-as.character(dat2$wkt)
dat2<-st_as_sf(dat2, wkt= "wkt",crs=27700)%>% st_transform(4326) # corrupt geom 7, 17, 18, 27, 28, 30
plot(dat2)

### These will need adding seperately to the map 
#[7] Watercourses (NPT SINC) and Taff and Rhondda Rivers (RCT SINCs)
#[17] Woodland SINCs (Ancient Woodland Sites)
# [27] Pen y Cymoedd Habitat Mangement Area (Central East)
#[28] Pen y Cymoedd Habitat Mangement Area (East)
# [30] Pen y Cymoedd Habitat Mangement Area (Central West)

dat1$x1<-st_coordinates(dat1)[,1]
dat1$y1<-st_coordinates(dat1)[,2]

dat1$Designation
library(leaflet)
library(RColorBrewer)

my_palette <- c("#e6194B","#3cb44b","#ffe119","#4363d8")

group<-unique(df$Designation)
summary(as.factor(group))

previewColors(colorFactor(my_palette, levels = group), group)
factpal <- colorFactor(my_palette, levels = group)
group
gc()
data1 = dat2[dat2$Designation == "HRA", ]
data2 = dat2[dat2$Designation == "SINC", ]
data3 = dat2[dat2$Designation == "SSSI", ]
list.files("C:\\Users\\C1751157\\Documents\\Lost peatlands\\", full.names = T, pattern =".shp")
data4 <- st_read("C:\\Users\\C1751157\\Documents\\Lost peatlands\\LP_watercourseSINCS.shp")%>%st_transform(4236)
dfwater<-df[7,]
data5 <- st_read("C:\\Users\\C1751157\\Documents\\Lost peatlands\\LP_nptWoodlandSINCs.shp" )%>%st_transform(4236)
dfwood<-df[17,]

list.files("C:\\Users\\C1751157\\Documents\\Lost peatlands\\2020_boundaries\\", full.names=T, pattern = "shp")
data6<-st_read("C:\\Users\\C1751157\\Documents\\Lost peatlands\\2020_boundaries\\Project area.shp")%>%st_transform(4326)

rm(map)

map <- leaflet() %>% setView(lng = -3.5729334, lat = 51.688395, zoom = 12) %>%
  addProviderTiles(providers$Esri.WorldImagery, "Aerial") %>%
  addPolygons(data = data6, opacity = 0.9, color = "#8B0000",
              fillColor = "transparent", group = "Lost Peatlands Project Boundary") %>%
  addPolygons(data = data1, label = data1$Site.Name,
                            popup = paste("Name: ", data1$Site.Name, "<br>",
                            "Description: ", data1$Description, "<br>",
                            "Designation: ", data1$Designation, "<br>",
                            "Condition: ", data1$Current.Management, "<br>",
                            "Ownership: ", data1$Ownership, "<br>",
                            "Threats: ", data1$Threats, "<br>",
                            "Opportunities: ",data1$Opportunity.for.the.Project, "<br>",
                            "Work proposed :", data1$Project.Management.Work.Proposed, "<br>",
                            "Programme Ref: ", data1$Management.Programme.Reference, "<br>",
                            "Further Info.: ", data1$Links.to.Futher.Project.Information.plans),
                            fillOpacity = 0.6, opacity = 0.05, 
                            fillColor = "#3cb44b", group = "HRA") %>%
  addPolygons(data = data2, label = "NPT SINC",
              popup = paste("Name: ", data2$Site.Name, "<br>",
                            "Description: ", data2$Description, "<br>",
                            "Designation: ", data2$Designation, "<br>",
                            "Condition: ", data2$Current.Management, "<br>",
                            "Ownership: ", data2$Ownership, "<br>",
                            "Threats: ", data2$Threats, "<br>",
                            "Opportunities: ",data2$Opportunity.for.the.Project, "<br>",
                            "Work proposed :", data2$Project.Management.Work.Proposed, "<br>",
                            "Programme Ref: ", data2$Management.Programme.Reference, "<br>",
                            "Further Info.: ", data2$Links.to.Futher.Project.Information.plans),
              fillOpacity = 0.6, opacity = 0.05, 
              fillColor = "#ffe119", group = "SINC") %>%
  addPolygons(data = data3, label = data3$Designation,
              popup = paste("Name: ", data3$Site.Name, "<br>",
                            "Description: ", data3$Description, "<br>",
                            "Designation: ", data3$Designation, "<br>",
                            "Condition: ", data3$Current.Management, "<br>",
                            "Ownership: ", data3$Ownership, "<br>",
                            "Threats: ", data3$Threats, "<br>",
                            "Opportunities: ",data3$Opportunity.for.the.Project, "<br>",
                            "Work proposed :", data3$Project.Management.Work.Proposed, "<br>",
                            "Programme Ref: ", data3$Management.Programme.Reference, "<br>",
                            "Further Info.: ", data3$Links.to.Futher.Project.Information.plans),
              fillOpacity = 0.6, opacity = 0.05, 
              fillColor = "#4363d8", group = "SSSI") %>%
  addCircleMarkers(data = dat1, ~x1, ~y1, label = "RCT SINC",
                   popup = paste("Name", dat1$Site.Name, "<br>",
                                 "Description:", dat1$Description, "<br>",
                                 "Designation:", dat1$Designation, "<br>",
                                 "Condition:", dat1$Current.Management, "<br>",
                                 "Ownership:", dat1$Ownership, "<br>",
                                 "Threats:", dat1$Threats, "<br>",
                                 "Opportunities:",dat1$Opportunity.for.the.Project, "<br>",
                                 "Work proposed:", dat1$Project.Management.Work.Proposed, "<br>",
                                 "Programme Ref:", dat1$Management.Programme.Reference, "<br>",
                                 "Further Info.:", dat1$Links.to.Futher.Project.Information.plans)
                   ,radius = 5, weight = 2, opacity = 0.02, fill = TRUE, fillOpacity = 0.6, 
                   color = "#ffe119",group = "SINC") %>%
  addPolygons(data = data4, label = "NPT SINC",
              popup = paste("Name: ", dfwater$Site.Name, "<br>",
                            "Description: ", dfwater$Description, "<br>",
                            "Designation: ", dfwater$Designation, "<br>",
                            "Condition: ", dfwater$Current.Management, "<br>",
                            "Ownership: ", dfwater$Ownership, "<br>",
                            "Threats: ", dfwater$Threats, "<br>",
                            "Opportunities: ",dfwater$Opportunity.for.the.Project, "<br>",
                            "Work proposed :", dfwater$Project.Management.Work.Proposed, "<br>",
                            "Programme Ref: ", dfwater$Management.Programme.Reference, "<br>",
                            "Further Info.: ", dfwater$Links.to.Futher.Project.Information.plans),
              fillOpacity = 0.6, opacity = 0.02, 
              fillColor = "#ffe119", group = "SINC") %>%
  addPolygons(data = data5, label = "NPT SINC",
              popup = paste("Name: ", dfwood$Site.Name, "<br>",
                            "Description: ", dfwood$Description, "<br>",
                            "Designation: ", dfwood$Designation, "<br>",
                            "Condition: ", dfwood$Current.Management, "<br>",
                            "Ownership: ", dfwood$Ownership, "<br>",
                            "Threats: ", dfwood$Threats, "<br>",
                            "Opportunities: ",dfwood$Opportunity.for.the.Project, "<br>",
                            "Work proposed :", dfwood$Project.Management.Work.Proposed, "<br>",
                            "Programme Ref: ", dfwood$Management.Programme.Reference, "<br>",
                            "Further Info.: ", dfwood$Links.to.Futher.Project.Information.plans),
              fillOpacity = 0.6, opacity = 0.2, 
              fillColor = "#ffe119", group = "SINC")
 


list.files("C:\\Users\\C1751157\\Documents\\Lost peatlands\\", full.names = T, pattern =".shp")
hmpce<-st_read("C:\\Users\\C1751157\\Documents\\Lost peatlands\\PyC_CE_Boundary_MS_20200615.shp")%>%st_transform(4326)
dfce<-df[26,]
hmpcw<-st_read("C:\\Users\\C1751157\\Documents\\Lost peatlands\\PyC_CW_Boundary_MS_20200615.shp")%>%st_transform(4326)
dfcw<-df[29,]
hmpw<-st_read("C:\\Users\\C1751157\\Documents\\Lost peatlands\\PyC_W_Boundary_MS_20200615.shp")%>%st_transform(4326)
dfw<-df[28,]
hmpe<-st_read("C:\\Users\\C1751157\\Documents\\Lost peatlands\\PyC_E_Boundary_MS_20200615.shp")%>%st_transform(4326)
dfe<-df[27,]
df$Site.Name
map<- map %>% 
  addPolygons(data=hmpce,popup = paste("Name: ", dfce$Site.Name, "<br>",
                                "Description: ", dfce$Description, "<br>",
                                "Designation: ", dfce$Designation, "<br>",
                                "Condition: ", dfce$Current.Management, "<br>",
                                "Ownership: ", dfce$Ownership, "<br>",
                                "Threats: ", dfce$Threats, "<br>",
                                "Opportunities: ",dfce$Opportunity.for.the.Project, "<br>",
                                "Work proposed :", dfce$Project.Management.Work.Proposed, "<br>",
                                "Programme Ref: ", dfce$Management.Programme.Reference, "<br>",
                                "Further Info.: ", dfce$Links.to.Futher.Project.Information.plans), 
                fillColor = "#f58231",fillOpacity = 0.5, 
              opacity = 0.1, label = "PyC Central East HMA",group = "PyC HMP") %>%
  addPolygons(data=hmpw,popup = paste("Name: ", dfw$Site.Name, "<br>",
                               "Description: ", dfw$Description, "<br>",
                               "Designation: ", dfw$Designation, "<br>",
                               "Condition: ", dfw$Current.Management, "<br>",
                               "Ownership: ", dfw$Ownership, "<br>",
                               "Threats: ", dfw$Threats, "<br>",
                               "Opportunities: ",dfw$Opportunity.for.the.Project, "<br>",
                               "Work proposed :", dfw$Project.Management.Work.Proposed, "<br>",
                               "Programme Ref: ", dfw$Management.Programme.Reference, "<br>",
                               "Further Info.: ", dfw$Links.to.Futher.Project.Information.plans),
              fillColor = "#f58231", fillOpacity = 0.1, 
              opacity = 0.3, label = "PyC West HMA",group = "PyC HMP") %>%
  addPolygons(data=hmpe,popup = paste("Name: ", dfe$Site.Name, "<br>",
                                  "Description: ", dfe$Description, "<br>",
                                  "Designation: ", dfe$Designation, "<br>",
                                  "Condition: ", dfe$Current.Management, "<br>",
                                  "Ownership: ", dfe$Ownership, "<br>",
                                  "Threats: ", dfe$Threats, "<br>",
                                  "Opportunities: ",dfe$Opportunity.for.the.Project, "<br>",
                                  "Work proposed :", dfe$Project.Management.Work.Proposed, "<br>",
                                  "Programme Ref: ", dfe$Management.Programme.Reference, "<br>",
                                  "Further Info.: ", dfe$Links.to.Futher.Project.Information.plans),
              fillColor = "#f58231", fillOpacity = 0.1, 
              opacity = 0.3, label = "PyC East HMA", group = "PyC HMP") %>%
  addPolygons(data=hmpcw,popup = paste("Name: ", dfcw$Site.Name, "<br>",
                                "Description: ", dfcw$Description, "<br>",
                                "Designation: ", dfcw$Designation, "<br>",
                                "Condition: ", dfcw$Current.Management, "<br>",
                                "Ownership: ", dfcw$Ownership, "<br>",
                                "Threats: ", dfcw$Threats, "<br>",
                                "Opportunities: ",dfcw$Opportunity.for.the.Project, "<br>",
                                "Work proposed :", dfcw$Project.Management.Work.Proposed, "<br>",
                                "Programme Ref: ", dfcw$Management.Programme.Reference, "<br>",
                                "Further Info.: ", dfcw$Links.to.Futher.Project.Information.plans),
              fillColor = "#f58231", fillOpacity = 0.1, 
              opacity = 0.3, label = "PyC Central West HMA",group = "PyC HMP") %>%
  addLayersControl(overlayGroups = c("Lost Peatlands Project Boundary", 
                                     "HRA","SSSI","SINC","PyC HMP"), 
                   options = layersControlOptions(collapsed = F))
map
?addLayersControl
library(htmlwidgets)
saveWidget(map, file="LPNaturalHeritage_20200616.html", selfcontained=FALSE)


# species records map ####

list.files("E:\\lost peatlands\\Field survey", full.names = T, pattern = ".csv")
df<-read.csv("E:\\lost peatlands\\Field survey/Lost Peatlands Master List2.csv")
summary(df)
df<-df%>%select(1:11)

dat<-df

dat<-filter(dat,!is.na(dat$x))
dat<-st_as_sf(dat, coords = c("x","y"),crs=27700)%>%st_transform(4326)
dat$x1<-st_coordinates(dat)[,1]
dat$y1<-st_coordinates(dat)[,2]

install.packages("taxize")
library("taxize")

taxon_king<-tax_name(as.character(df$Family), get = "kingdom", db = "ncbi")

dat$kingdom<-taxon_king$kingdom
summary(as.factor(dat$kingdom))

foo<-dat[is.na(dat$kingdom),]
foo1<-dat[!is.na(dat$kingdom),]
foo$Order
taxon_king<-tax_name(as.character(foo$Order), get = "kingdom", db = "ncbi")
foo$kingdom<-taxon_king$kingdom

dat1<-rbind(foo, foo1)
dat<-dat1
rm(dat1)
dat$broadGroup<-dat$kingdom
summary(as.factor(dat$kingdom))
dat[dat$Common.Name == "a lichen",]$kingdom<-"Fungi"
dat[dat$Common.Name == "money spider",]$kingdom<-"Metazoa"
dat[dat$Common.Name == "a slime mould",]$kingdom<-"Fungi"

dat[dat$kingdom == "Metazoa",]$broadGroup<-"Animal"
dat[dat$kingdom == "Viridiplantae",]$broadGroup<-"Bryophytes and Flowering Plants"
dat[dat$kingdom == "Fungi",]$broadGroup<-"Fungi"

fee<-dat$broadGroup


library(leaflet)
library(RColorBrewer)
previewColors(colorFactor("Paired", domain = NULL), LETTERS[1:9])
my_palette <- c("#e6194B",
                "#B3B6B7","#E5E9EF")

group<-unique(dat$broadGroup)

previewColors(colorFactor(my_palette, levels = group), group)
factpal <- colorFactor(my_palette, levels = group)
group
map = leaflet(dat) %>% addTiles()
for (i in group) {
  data = dat[dat$broadGroup == i, ]
  map = map %>% addCircleMarkers(data = data, ~x1, ~y1, 
                                 popup = paste("Common Name", data$Common.Name, "<br>"),
                                 radius = 5, weight = 2, opacity = 0.5, fill = TRUE, 
                                 fillOpacity = 0.2, color = ~factpal(group), 
                                 group = i,clusterOptions = 
                                   markerClusterOptions()) %>% 
    addPolygons(data=aoi84, popup = aoi84$Site, 
                fillColor = "#3cb44b", 
                fillOpacity = 0.1, opacity = 0.05)
}
map<-map %>% addLayersControl(overlayGroups = group, 
                              options = layersControlOptions(collapsed = FALSE))
map

mapview(dat)

library(htmlwidgets)
saveWidget(map, file="LostPeatlands_naturalheritage_map1_20200601.html", selfcontained=FALSE)

addPopupImages()