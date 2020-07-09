# 16_lost peatlands - peat depth with Rayshader

# To install the latest version from Github:
# install.packages("devtools")
devtools::install_github("tylermorganwall/rayshader")

library(raster)
library(rayshader)
cregp<-raster(file.choose())
cregp

wgs84<-crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

creg1<-raster::aggregate(cregp, fact=20)
plot(creg1)
creg84<-projectRaster(creg1, crs = wgs84, method = "bilinear")
plot(creg84)

bbbox<-extent(creg84)
bbox <- list(
  p1 = list(long = -3.504242, lat = 51.658861),
  p2 = list(long = -3.710432, lat = 51.72576)
)

library(leaflet)
leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
    fillColor = "transparent"
  ) %>%
  fitBounds(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
  )

define_image_size <- function(bbox, major_dim) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}
image_size <- define_image_size(bbox, major_dim = 600)

elev_matrix <- matrix(
  raster::extract(creg84, raster::extent(creg84), buffer = 1000), 
  nrow = ncol(creg84), ncol = nrow(creg84)
)

elev_matrix2 <- matrix(
  raster::extract(creg84, raster::extent(creg84), buffer = 1000), 
  nrow = ncol(creg84), ncol = nrow(creg84)
)
# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = 10)
raymat <- ray_shade(elev_matrix, zscale = 10, lambert = TRUE)

# plot 2D ####
elev_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_overlay(overlay_img)%>%
  plot_map()
min(elev_matrix)

elev_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_overlay(overlay_img)%>%
  plot_3d(elev_matrix, zscale = 2000)

#Peat plt ####
rgl::clear3d()
elev_matrix %>% 
  sphere_shade(texture = "unicorn", colorintensity = 15) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_overlay(overlay_img) %>%
  plot_3d(elev_matrix, zscale = 0.5, windowsize = c(1200, 1000), solid=F, solidcolor = "grey0",
          theta = 25, phi = 30, zoom = 0.65, fov = 60)
render_label(heightmap=elev_matrix, x = 110, y = 190, z = 100, 
             zscale = 2, text = "Remnant Bog failed crop", freetype = F)
render_label(heightmap=elev_matrix, x = 150, y = 350, z = 100, 
             zscale = 2, text = "Carn Caglau cairn - Bronze age", freetype = F)
render_snapshot()


# with overlay map ####
?slippy_overlay
#install.packages("geoviz")
library(geoviz)
overlay_img <-
  slippy_overlay(creg84,
                 image_source = "mapbox",
                 image_type = "satellite",
                 png_opacity = 0.9,
                 max_tiles = 20,
                 api_key = "pk.eyJ1IjoibWlrZXNoZXdyaW5nIiwiYSI6ImNrMjNyZHZzZTBtcmYzbm1xaXJxZWFqa2kifQ.DpLKYK5GOVtsQT9a5gkhrQ")

rgl::clear3d()
?plot_3d
elev_matrix %>% 
  sphere_shade(colorintensity = 10) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_overlay(overlay_img) %>%
  plot_3d(elev_matrix, zscale = 3000, windowsize = c(1200, 1000),
          theta = 25, phi = 30, zoom = 0.8, fov = 60) # zoom 0.65
##render_label(heightmap=elev_matrix, x = 110, y = 190, z = 100, 
   #          zscale = 2, text = "Hwb", freetype = F)
#render_label(heightmap=elev_matrix, x = 150, y = 350, z = 100, 
#             zscale = 2, text = "Carn Caglau cairn - Bronze age", freetype = F)
render_movie(filename = filename_movie, fps = 30, title_text = "Pen y Cymoedd Wind Farm")
filename_movie = tempfile()
?render_movie
