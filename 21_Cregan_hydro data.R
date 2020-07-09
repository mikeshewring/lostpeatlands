# cregan water levels
library(ggplot2)
library(tidyverse)
library(stringr)
library(sf)
library(mapview)
library(lubridate)
library(plotly)
df<-read.csv(file.choose())
df$date_time<-dmy_hm(df$date_time)
foo<-pivot_longer(df, 2:10, names_to = "Dipwell", values_to =  "mBGL")
foo$Dipwell
ggplot()+
  geom_line(data= foo[foo$Dipwell == "CR1",],aes(x=date_time, y=mBGL),col= "#0000FF", alpha = 0.5)+
  geom_line(data= foo[foo$Dipwell == "CR2",],aes(x=date_time, y=mBGL),col= "#008000", alpha = 0.5)+
  geom_line(data= foo[foo$Dipwell == "CR3",],aes(x=date_time, y=mBGL),col= "#ffdb00", alpha = 0.5)+
  geom_line(data= foo[foo$Dipwell == "CR4",],aes(x=date_time, y=mBGL),col= "#575759", alpha = 0.5)+
  geom_line(data= foo[foo$Dipwell == "CR5",],aes(x=date_time, y=mBGL),col= "#eb09c1", alpha = 0.5)+
  xlab("") +
  theme_minimal()
?geom_line
(p<-ggplot(data= foo [foo$Dipwell == "CR4" |foo$Dipwell == "CR5" |foo$Dipwell == "CR6",])+
  geom_line(aes(x=date_time, y=mBGL, colour = Dipwell), alpha = 0.5)+
  scale_colour_manual(values=c("#fadbe0","#eaadbd","#876880"))+
  xlab("") +
  theme_minimal())
p
p <- ggplotly(p)
p
library(htmlwidgets)
saveWidget(p, "CreganDipwellPlot4-6.html")
