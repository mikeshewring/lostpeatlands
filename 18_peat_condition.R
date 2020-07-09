library(ggplot2)
library(tidyverse)
library(stringr)
library(sf)
library(mapview)

pc<-st_read(file.choose())
head(pc)
summary(pc$LPSW.IUCN)
plot(pc$LPSW.IUCN)
sum<-pc%>%group_by(LPSW.IUCN)%>%summarise(cnt=n())
sum(sum$cnt)
ggplot(data = sum)+
  geom_col(aes(x=LPSW.IUCN, y=cnt, fill = LPSW.IUCN))+
  scale_fill_viridis_d(name="Peat Condition",
                       breaks=c("Drained/Afforested", "Drained/Afforested/Road edge", "Drained/Bryo",
                                "Drained/Vpl","NA"),
                       labels=c("Drained afforested", "Road edge", "Drained bryophyte dominated",
                                "Drained vascular plant dominated", "No category"), alpha = 0.7)+
  theme_classic()+
  xlab("Peat Condition Category")+
  ylab("No. of Points")+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

                 