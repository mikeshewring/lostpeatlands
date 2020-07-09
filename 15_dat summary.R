library(ggplot2)
library(tidyverse)
library(stringr)

CN<-read.csv(file.choose())
CN$site<-"cregan"#cregan
CN$pnts<-93
CW<-read.csv(file.choose())
CW$site<- "cwm saerbren" #
CW$pnts<-22
CS<-read.csv(file.choose())
CS$site<-"castell nos" #113
CS$pnts<-113
summary(df)
df<-rbind(CN,CW,CS)  
df$ab2<-df$abun/df$pnts

plot(df$ab2~df$genus)

spsum<-df%>%group_by(genus, site)%>%summarise(cnt=n(), absum=sum(ab2))
unique(spsum$Species)

ggplot(data = spsum)+
  geom_point(aes(x=genus,y=absum, shape=site, col=site, size=absum))+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  ylab("Relative Abundance")+
  theme(axis.text.x=element_text(angle=90,hjust=1))
