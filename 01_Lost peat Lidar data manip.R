#### 01_Lost peat Lidar data manip.
## 
# Mike Shewring
# 16 09 2019

# script taking raster lidar tiles and mosaicing these into a single file and calculate slope values from these

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


# Cregan ####
aoi<-st_read("E:\\lost peatlands\\Lost peatlands.shp")
aoi
aoi$Type
HRA<-filter(aoi, Type == "Habitat Restoration Area" )
HRA
plot(HRA[1])

# filter to aoi
creganshp<-filter(HRA, Site == "Cregan HRA")
creganshp<-st_buffer(creganshp, 200)
plot(creganshp)


# load cregan rasters
list.files("E:\\lost peatlands\\NRW_lidar\\cregan\\dtm0.5m", full.names = T)
cregan<-raster("E:\\lost peatlands\\NRW_lidar\\cregan\\dtm0.5m/V0207410.asc")
cregan2<-raster("E:\\lost peatlands\\NRW_lidar\\cregan\\dtm0.5m/V0207421.asc")
cregan3<-raster("E:\\lost peatlands\\NRW_lidar\\cregan\\dtm0.5m/V0207424.asc")
cregan4<-raster("E:\\lost peatlands\\NRW_lidar\\cregan\\dtm0.5m/V0207425.asc")
cregan5<-raster("E:\\lost peatlands\\NRW_lidar\\cregan\\dtm0.5m/V0207406.asc")
cregan6<-raster("E:\\lost peatlands\\NRW_lidar\\cregan\\dtm0.5m/V0207327.asc")
creganm1<-raster::merge(cregan,cregan2,cregan3,cregan4,cregan5,cregan6)
crs(creganm1)

crs(creganm1)<-"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"

# crop
creganm2<-crop(creganm1, creganshp)
creganm2<-mask(creganm2, creganshp)
plot(creganm2)
creganm2<-creganm2/1000
writeRaster(creganm2, "E:\\lost peatlands\\NRW_lidar\\cregan\\cregan_0.5_lidar1.tif", overwrite=T)

cregan<-terrain(creganm2, opt='slope',unit='degrees', neighbors = 8)
plot(cregan)


writeRaster(creganm2, "E:\\lost peatlands\\NRW_lidar\\cregan\\cregan_0.5_lidar1.tif", overwrite=T)

cregan<-terrain(creganm2, opt='slope',unit='degrees', neighbors = 8)
plot(cregan)

writeRaster(cregan, "E:\\lost peatlands\\NRW_lidar\\cregan\\cregan_0.5m_slope1.tif", overwrite=T)

# lets make some contours ## whoop whoop
creg<-raster("E:\\lost peatlands\\NRW_lidar\\cregan\\mosaic\\cregan_0.5_lidar.tif")
plot(creg)

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
link = link2GI::linkGRASS7(x = creg, 
                    default_GRASS7 = my_default_GRASS7, 
                    quiet = FALSE)
?linkGRASS7
# 1. convert raster to spatial grid data frame
g <- as(creg, 'SpatialGridDataFrame')
library(rgrass7)
library(magrittr)
library(sp)

use_sp()
# 2. load raster into GRASS data base
writeRAST(g, flags='overwrite',vname = "creg_DSM")
execGRASS("r.info", map="creg_DSM")
# 3.  use Grass r.contour
execGRASS(cmd='r.contour', flags='overwrite', 
          parameters =  list(input='creg_DSM', output = 'conts4',
                             step = 1, cut = 1000))

execGRASS(cmd='v.generalize', flags='overwrite', 
          parameters =  list(input='conts4', output = 'conts5',
                             method = 'boyle', threshold = 0, look_ahead=500))

# 4. extract to SDF
contours <- readVECT("conts4", plugin=NULL) # sigma 
contours1 <- readVECT("conts5", plugin=NULL) # sigma 

# 5. vis
par(mfrow=c(1,2))
plot(contours)
plot(contours1)
par(mfrow=c(1,1))

# 6. convert to sf and export
contours<-st_as_sf(contours)
contours1<-st_as_sf(contours1)
par(mfrow=c(1,2))
plot(contours[2])
plot(contours1[2])
par(mfrow=c(1,1))
st_write(contours, "Cregan_1_contours20200326.shp")
st_write(contours1, "Cregan_1_contourssmooth20200326.shp")

rm(g,creg,contours,contours1)
citation('rgrass7')

# Castell Nos ####
# aoi<-st_read("E:\\lost peatlands\\Lost peatlands.shp")
aoi
aoi$Type
HRA<-filter(aoi, Type == "Habitat Restoration Area" )
HRA

# filter to aoi
shp<-filter(HRA, Site == "Castell Nos HRA")
shp<-st_buffer(shp, 200)
plot(shp)
viewExtent(shp)
st_write(shp, "E:\\lost peatlands\\castellnos_HRA.shp")


# load cregan rasters
l<-list.files("E:\\lost peatlands\\NRW_lidar\\castellnos\\dtm.asc", full.names = T)
l
r<-raster(l[1])
r2<-raster(l[2])
r3<-raster(l[3])
r4<-raster(l[4])
r5<-raster(l[5])
r6<-raster(l[6])

m1<-raster::merge(r,r2,r3,r4,r5,r6)
crs(m1)
m1
crs(m1)<-"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"

plot(m1)
plot(shp[1], add=T)
shp

# crop
m2<-crop(m1, shp)
m2<-mask(m2, shp)
plot(m2)
m2<-m2/1000
writeRaster(m2, "E:\\lost peatlands\\NRW_lidar\\castellnos\\dtm.asc\\castellnos_0.5_lidar1.tif", overwrite=T)

slope<-terrain(m2, opt='slope',unit='degrees', neighbors = 8)
plot(slope)

writeRaster(slope, "E:\\lost peatlands\\NRW_lidar\\castellnos\\castellnos_0.5m_slope1.tif", overwrite=T)

viewExtent(slope)

# Cwm saerbren ####
# aoi<-st_read("E:\\lost peatlands\\Lost peatlands.shp")
aoi
aoi$Type
HRA<-filter(aoi, Type == "Habitat Restoration Area" )
HRA

# filter to aoi
shp<-filter(HRA, Site == "Cwm Saerbren HRA")
shp<-st_buffer(shp, 200)
plot(shp)
viewExtent(shp)
st_write(shp, "E:\\lost peatlands\\CwmSaerbren_HRA.shp")


# load cregan rasters
l<-list.files("E:\\lost peatlands\\Lle_lidar\\cwmsaerbrenlidar\\", full.names = T)
l
r<-raster(l[2])
r2<-raster(l[3])
r3<-raster(l[4])
r4<-raster(l[5])
r5<-raster(l[6])


m1<-raster::merge(r,r2,r3,r4,r5)
crs(m1)
m1
crs(m1)<-"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"

plot(m1)
plot(shp[1], add=T)
shp

# crop
m2<-crop(m1, shp)
m2<-mask(m2, shp)
plot(m2)
#m2<-m2/1000
writeRaster(m2, "E:\\lost peatlands\\Lle_lidar\\cwmsaerbrenlidar\\cwmsaerbren_0.5_lidar1.tif", overwrite=T)

slope<-terrain(m2, opt='slope',unit='degrees', neighbors = 8)
plot(slope)

writeRaster(slope, "E:\\lost peatlands\\Lle_lidar\\cwmsaerbrenlidar\\cwmsaerbren_0.5m_slope1.tif", overwrite=T)

viewExtent(slope)
