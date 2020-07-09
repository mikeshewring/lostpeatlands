# 03_lost peat - desk study records

library(dplyr)
library(readr)
library(spatialEco)
library(raster)
library(rgdal)
library(gdalUtils)
library(sf)
library(tidyverse)
library(mapview)

#

d <- list.files("E:\\lost peatlands\\desk study\\protected species", pattern = ".csv",full.names = TRUE) #%>% lapply(read_csv) %>%  bind_rows ()
d
df<-read.csv("E:\\lost peatlands\\desk study\\protected species\\NPT/Cregan Species 2km Buffer Filtered.csv")
df
head(df)
tail(df)

# remove na/ blank rows
foo<-df[!is.na(df),]
head(foo)
tail(foo)
foo<-foo[!is.na(foo$taxon),]
summary(foo$taxon)
foo$taxon1<-as.character(foo$taxon)
# export
write.csv(foo, "NPTcregan2kmspecies.csv")

# designations - 
jncc<-read.csv("E:\\lost peatlands\\desk study\\protected species/JNCC_designations.csv")
jncc$taxon1<-as.character(jncc$Designated.name)

fee<-left_join(foo, jncc, by = "taxon1" )
unique(fee$taxon1)

fee$Designation.abbreviation
# plots
plot(foo$taxon)

# export species lists
specieslist<-unique(foo$taxon)
specieslist

specieslist1<-unique(foo$vernacular)
specieslist1


