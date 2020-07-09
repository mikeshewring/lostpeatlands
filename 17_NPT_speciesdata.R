library(sf)
library(mapview)
library(tidyverse)
dat<-st_read(file.choose())
summary(dat)
summary(dat$taxon)
unique(dat$vernacular)

# Birds ####
b<-read.csv(file.choose())
df <- dat[dat$taxon %in% b$Scientific.name,]
plot(df[8], axes = T)
df$vernacular<-as.factor(as.character(df$vernacular))
foo<-summary(df$vernacular)
fee<-unique(df$vernacular)
write.csv(fee, "creganbirds.csv")
write.csv(foo, "creganbirdsno.csv")
getwd()



mapView(df, label = df$vernacular)

# inverts ####
dat<-st_read(file.choose())
summary(dat)
summary(dat$taxon)
unique(dat$vernacular)

b<-read.csv(file.choose())#
b$Recommended_taxon_name<-as.factor(as.character(b$Recommended_taxon_name))
df <- dat[dat$taxon %in% b$Recommended_taxon_name,]
plot(df[8], axes = T)
df$vernacular<-as.factor(as.character(df$vernacular))

inv<-df%>%group_by(taxon,vernacular)%>%summarise(cnt = n())
write.csv(inv, "creganinverts.jncc.count.csv")

stat<- b[b$Recommended_taxon_name %in% dat$taxon,]
stat$taxon<-as.factor(as.character(stat$Recommended_taxon_name))
summary(stat$F..Global.Red.list.status)
inv1<-left_join(inv, stat, by = "taxon")
inv1$geometry<-NULL
write.csv(inv1, "creganinverts.jncc.count2.csv")

write.csv(fee, "creganinverts.jncc.csv")
fee<-unique(as.factor(as.character(df$taxon)))
fee<-unique(df$vernacular)
fee
write.csv(fee, "creganinverts.jncc.csv")
write.csv(df, "creganinvertsjncc1.csv")
getwd()



mapView(df, label = df$taxon)
# plants ####
dat<-st_read(file.choose())
summary(dat)
summary(dat$taxon)
unique(dat$vernacular)

b<-read.csv(file.choose())#
b$Recommended_taxon_name<-as.factor(as.character(b$Recommended_taxon_name))
df <- dat[dat$taxon %in% b$Recommended_taxon_name,]
plot(df[8], axes = T)
df$vernacular<-as.factor(as.character(df$vernacular))

inv<-df%>%group_by(taxon,vernacular)%>%summarise(cnt = n())
write.csv(inv, "creganplantas.jncc.count.csv")

stat<- b[b$Recommended_taxon_name %in% dat$taxon,]
stat$taxon<-as.factor(as.character(stat$Recommended_taxon_name))
summary(stat$F..Global.Red.list.status)
inv1<-left_join(inv, stat, by = "taxon")
inv1$geometry<-NULL
write.csv(inv1, "creganplants.jncc.count2.csv")


