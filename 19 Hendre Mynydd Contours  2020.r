# Hendre Mynydd Contours  2020
#### Required Packages ####
# install.packages("spatialeco")
install.packages("rgrass7")
library(spatialEco)
library(raster)
library(rgdal)
library(gdalUtils)
library(rgrass7)
library(link2GI)
library(sf)
library(tidyverse)


# Hendre Mynydd Carpark ####
aoi<-st_read("E:\\lost peatlands\\classroom.shp")
aoi
plot(aoi)
shp<-st_buffer(aoi, 200)

# load cregan rasters
r<-raster("E:\\PyC LIDAR\\P_11352_11353_smooth\\PYC_dtm_mosaic.tif")
plot(r)

crs(r)<-"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"

# crop
m<-crop(r, shp)
m<-raster::mask(m, shp)
plot(m)
m<-m/1000
writeRaster(m, "E:\\lost peatlands\\NRW_lidar\\classroom_0.5_lidar1.tif", overwrite=T)

m<-raster( "E:\\lost peatlands\\NRW_lidar\\classroom_0.5_lidar1.tif")
# lets make some contours ## whoop whoop
# lets try and set env vars for GRASS ####
# Set PYTHONPATH
Sys.setenv(PYTHONPATH = paste0("C:\\OSGEO4~1\\apps\\grass\\grass-7.0.3\\etc\\python;C:\\OSGEO4~1\\apps\\grass\\grass-7.0.3\\gui\\wxpython"))

# Set PYTHONHOME
Sys.setenv(PYTHONHOME = "C:\\OSGEO4~1\\apps\\Python27")

# Set PATHS for case of OSGEO4W
newPath <- "C:\\OSGeo4W64\\bin"
Sys.setenv("PATH" = paste(newPath, Sys.getenv("PATH"), sep=.Platform$path.sep))
newPath <- "C:\\OSGeo4W64\\apps\\grass\\grass78\\lib"
Sys.setenv("PATH" = paste(newPath, Sys.getenv("PATH"), sep=.Platform$path.sep))
newPath <- "C:\\OSGeo4W64\\apps\\grass\\grass78\\bin"
Sys.setenv("PATH" = paste(newPath, Sys.getenv("PATH"), sep=.Platform$path.sep))
newPath <- "C:\\OSGeo4W64\\apps\\Python37\\Scripts"
Sys.setenv("PATH" = paste(newPath, Sys.getenv("PATH"), sep=.Platform$path.sep))

# Set GISBASE
# (directory where GRASS lives)
Sys.setenv(GISBASE = "C:\\OSGeo4W64\\apps\\grass\\grass78")

# Set GRASS_PYTHON
# (To override Python executable)
Sys.setenv(GRASS_PYTHON = "C:\\OSGeo4W64\\bin\\python3.exe")
link = findGRASS("E:")

my_default_GRASS7 <- c("C:/OSGeo4W64/","grass78","osgeo4W") # here GRASS GIS 7.6 (Windows)
my_default_GRASS7_path <- link2GI::paramGRASSw(my_default_GRASS7, FALSE)$gisbase_GRASS

# get start path of working directory
path.StartWD <- getwd()

# this a quick link to get GRASS database running in your spatial projection and region
link = link2GI::linkGRASS7(x = m, 
                           default_GRASS7 = my_default_GRASS7, 
                           quiet = FALSE)

# 1. convert raster to spatial grid data frame
g <- as(m, 'SpatialGridDataFrame')
library(rgrass7)
library(magrittr)
library(sp)

use_sp()
# 2. load raster into GRASS data base
writeRAST(g, flags='overwrite',vname = "DSM")
execGRASS("r.info", map="DSM")
# 3.  use Grass r.contour
execGRASS(cmd='r.contour', flags='overwrite', 
          parameters =  list(input='DSM', output = 'conts4',
                             step = 0.5, cut = 1000))

execGRASS(cmd='v.info', flags=, 
          parameters =  list(map='conts4'))

# 3a. use grass to output to dxf
execGRASS(cmd='v.out.dxf', flags='overwrite', 
          parameters =  list(input='conts4', output = '"E:\\lost peatlands\\NRW_lidar\\classroom.dxf'))

# 4. extract to SDF
contours <- readVECT("conts4", plugin=NULL, pointDropZ = F) # sigma 


# 5. vis
plot(contours)

# 6. convert to sf and export
contours<-st_as_sf(contours)
plot(contours[2])
st_write(contours, "Classroom_0.5_contours20200511.shp")

rm(g,creg,contours,contours1)
citation('rgrass7')
