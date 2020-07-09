# dipwell data modelling
library(GSODR)
library(tidyverse)
library(lubridate)
library(mgcv)
library(ggplot2)
# preciptation data ####
tbar<-nearest_stations(51.706771,-3.5876356, 50)
tdat<-get_GSOD(years = 2014:2018, tbar)
tdat<-tdat%>%filter(STN_NAME =="SENNYBRIDGE NO2")%>%select(YEAR, MONTH, DAY, YDAY, PRCP, TEMP, WDSP)
tdat$date<-dmy(paste0(tdat$DAY,tdat$MONTH, tdat$YEAR))
tdat$date1<-tdat$date
head(tdat)
plot(tdat$PRCP~as.factor(tdat$MONTH))
hist(tdat$PRCP)

write.csv(tdat, "E:\\PyC\\dipwell_data\\rain\\PRCPTEMPData.csv")
tdat<-read.csv("E:\\PyC\\dipwell_data\\rain\\PRCPData.csv")
tdat$date1<-ymd(tdat$date1)
# dipwell data - load mu;ltiple files and merge ####
list.files("E:\\PyC\\dipwell_data", pattern = "csv", full.names = T)
dip1<-read.csv("E:\\PyC\\dipwell_data/T34_E01_dipwell_data.csv")
dip2<-read.csv("E:\\PyC\\dipwell_data/T49_dipwell_data.csv")
dip3<-read.csv("E:\\PyC\\dipwell_data/T50_T46_dipwell_data.csv")
dip4<-read.csv("E:\\PyC\\dipwell_data/T52_T53_dipwell_data.csv")
dip5<-read.csv("E:\\PyC\\dipwell_data/T54_dipwell_data.csv")
dip6<-read.csv("E:\\PyC\\dipwell_data/T26_29_dipwell_data.csv")
dip1a<-gather(dip1, key = dipwell, value = date  )
dip2a<-gather(dip2, key = dipwell, value = date  )
dip3a<-gather(dip3, key = dipwell, value = date  )
dip4a<-gather(dip4, key = dipwell, value = date  )
dip5a<-gather(dip5, key = dipwell, value = date  )
dip6a<-gather(dip6, key = dipwell, value = date  )
dip1a$date1<-rep(dip1$date,12)
dip2a$date1<-rep(dip2$date,12)
dip3a$date1<-rep(dip3$date,34)
dip4a$date1<-rep(dip4$date,33)
dip5a$date1<-rep(dip5$date,14)
dip6a$date1<-rep(dip6$date,53)
dip1a$area<-"T34"
dip2a$area<-"T49"
dip3a$area<-"T46_T50"
dip4a$area<-"T52_53"
dip5a$area<-"T54"
dip6a$area<-"T26_T29"
dip1a$waterdepth<-dip1a$date
dip1a$date<-NULL
dip2a$waterdepth<-dip2a$date
dip2a$date<-NULL
dip3a$waterdepth<-dip3a$date
dip3a$date<-NULL
dip4a$waterdepth<-dip4a$date
dip4a$date<-NULL
dip5a$waterdepth<-dip5a$date
dip5a$date<-NULL
dip6a$waterdepth<-dip6a$date
dip6a$date<-NULL
rm(dip1,dip2,dip3,dip4, dip5, dip6)
dip<-bind_rows(dip1a,dip2a,dip3a,dip4a, dip5a, dip6a)
rm(dip1a,dip2a,dip3a,dip4a,dip5a, dip6a)

# now sort dates ####

dip$date1<-dmy(dip$date1)
dip$month<-as.factor(month(dip$date1))
dip$jday<-yday(dip$date1)
dip$year<-year(dip$date1)
summary(dip$year)

# merge datasets
foo<-left_join(dip, tdat, by = "date1")
week_sum_precip <- tdat %>%
  mutate(week = lubridate::week(date1)) %>%
  group_by(week, YEAR) %>% # group by the day column
  summarise(CPRCP=sum(PRCP)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()
week_sum_temp <- tdat %>%
  mutate(week = lubridate::week(date1)) %>%
  group_by(week, YEAR) %>% # group by the day column
  summarise(MTEMP=mean(TEMP)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()

week_mean_wind <- tdat %>%
  mutate(week = lubridate::week(date1)) %>%
  group_by(week, YEAR) %>% # group by the day column
  summarise(MWDSP=mean(WDSP)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()

week_sum_precip$YEAR<-as.character(week_sum_precip$YEAR)
week_sum_temp$YEAR<-as.character(week_sum_temp$YEAR)
week_mean_wind$YEAR<-as.character(week_mean_wind$YEAR)
foo$week<-week(foo$date1)
foo$YEAR<-as.character(foo$year)
foo<-left_join(foo, week_sum_precip, by = c("week","YEAR"))
foo<-left_join(foo, week_sum_temp, by = c("week","YEAR"))
foo<-left_join(foo, week_mean_wind, by = c("week","YEAR"))

write.csv(foo, "E:/PyC/PyCdipwelldata_20191016.csv")
foo<-read.csv("E:/PyC/PyCdipwelldata_20191016.csv")

# data explore ####
plot(dip$waterdepth~dip$date1)
plot(dip$waterdepth~dip$month)

foo$YEAR<-as.factor(foo$YEAR)
foo$month<-as.factor(foo$month)

ggplot(data=dip)+
  geom_point(aes(x=date1, y = waterdepth))+
  geom_smooth(aes(x=date1, y = waterdepth))+
  theme_minimal()


ggplot(data =foo)+
  geom_boxplot(aes(y=waterdepth, x=month),
               fill= "green", outlier.colour = "blue",outlier.shape = 1, notch=T)+
  facet_wrap(~YEAR)+
  theme_minimal()

ggplot(data =foo)+
  geom_boxplot(aes(y=waterdepth, x=month),
               fill= "green", outlier.colour = "blue",outlier.shape = 1, notch=T)+
  facet_wrap(~area)+
  theme_minimal()
# precipitation data explore ####

tdat$YEAR<-as.factor(tdat$YEAR)
tdat$MONTH<-as.factor(tdat$MONTH)
# by month over all years
ggplot(data =tdat)+
  geom_boxplot(aes(y=PRCP, x=MONTH),
               fill= "blue", outlier.colour = "red",outlier.shape = 1, notch=T)+
  theme_minimal()

# by month and year
ggplot(data =tdat)+
  geom_boxplot(aes(y=PRCP, x=MONTH),
               fill= "blue", outlier.colour = "red",outlier.shape = 1)+
  facet_wrap(~YEAR)+
  theme_minimal()

# cumulative summarise by month
Month_sum_precip <- tdat %>%
  group_by(MONTH, YEAR) %>% # group by the day column
  summarise(CPRCP=sum(PRCP)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()
#  cumulative precipitation by month and year
ggplot(data =Month_sum_precip)+
  geom_col(aes(y=CPRCP, x=MONTH),
               fill= "blue", )+
  facet_wrap(~YEAR)+
  theme_minimal()

# cumulative precipitation by month 
ggplot(data =Month_sum_precip)+
  geom_col(aes(y=CPRCP, x=MONTH),
           fill= "blue", )
  theme_minimal()

# fit models ####
# is response variable normall distributed?
hist(foo$waterdepth)
shapiro.test(foo$waterdepth)
shapiro.test(rnorm(100, mean = -0.2, sd = 3))
shapiro.test(foo$waterdepth)# W = 0.91504, p-value < 2.2e-16 - not normal

# transform to normality? - #### does not work####
e<-foo[!is.na(foo$waterdepth),]
e$waterdepth<-e$waterdepth*-1
# shapiro normaility test
shapiro.test(exp(e$waterdepth))
hist(exp(e$waterdepth))
shapiro.test(log10(e$waterdepth))
hist(log10(foo$waterdepth))
shapiro.test(sqrt(e$waterdepth))
hist(sqrt(foo$waterdepth))
shapiro.test(e$waterdepth^3)
hist(foo$waterdepth^3)

# model1  day only ####

model1<-gam(waterdepth~s(jday, k=-1, bs="cc", by=as.factor(YEAR)), 
            data = foo, method = "REML")
model1

plot.gam(model1)

summary(model1) # R-sq.(adj) =   0.26   Deviance explained = 26.6%
gam.check(model1) 
AIC(model1) #-4577.285

# model 2 - including PRCP
model2<-gam(waterdepth~s(jday,k=-1,bs="cc", by=as.factor(YEAR))+scale(CPRCP), 
            data = foo, na.rm=T,method = "REML")
model2

plot(model2)
summary(model2) #R-sq.(adj) =  0.266   Deviance explained = 27.2%

gam.check(model2) 
AIC(model2) #-4314.57 model is worse with scaled precipitation?


# model 3 includeing cumulative rain and temp

model3<-gam(waterdepth~s(jday,k=-1,bs="cc", by=as.factor(YEAR))+
              scale(CPRCP)+scale(MTEMP), 
            data = foo, method = "REML",na.rm=T)
model3
gam.check(model3)

plot(model3, residuals=T)
summary(model3) # R-sq.(adj) =  0.274   Deviance explained = 28.1%

gam.check(model3) #residuals look pretty good, qq plot poor
AIC(model3)# -4337.415  #decline in performance....

# model 4 includeing cumulative rain and temp and wdsp

model4<-gam(waterdepth~s(jday,k=-1,bs="cc", by=as.factor(YEAR))+
              scale(CPRCP)+scale(MTEMP)+scale(MWDSP), 
            data = foo, method = "REML",na.rm=T)
model4

plot(model4, residuals=T)
summary(model4) # R-sq.(adj) =  0.265   Deviance explained = 27.2%

gam.check(model4) #residuals look pretty good, qq plot poor
AIC(model4) #-4340.561 decline in perfromance

# model 5 chamnge error familt to a truncated normal and add in interactions

model5<-gam(waterdepth~s(jday,k=-1,bs="cc", by=as.factor(YEAR))+
              scale(CPRCP)+scale(MTEMP)+scale(MWDSP)+
              scale(CPRCP):scale(MTEMP):scale(MWDSP), 
            family=scat(link="identity"), 
            data = foo, method = "REML",na.rm=T)
model5

#plot(model5, residuals=T) # WTF?
summary(model5) # R-sq.(adj) =  0.281   Deviance explained = 23.4%

gam.check(model5) #residuals look pretty good, qq plot poor
AIC(model5) #-4965.884 better perfromance

# model 6 chamnge error familt to a truncated normal... i think

model6<-gam(waterdepth~s(jday,k=-1,bs="cc", by=as.factor(YEAR))+
              scale(CPRCP)+scale(MTEMP)+scale(MWDSP)+
              scale(CPRCP):scale(MTEMP):scale(MWDSP)+
              as.factor(area), 
            family=scat(link="identity"), 
            data = foo, method = "REML",na.rm=T)
model6

#plot(model6, residuals=T) # WTF?
summary(model6) #R-sq.(adj) =  0.307   Deviance explained = 25.6%

gam.check(model6) #residuals look pretty good, qq plot poor
AIC(model6) #-5065.157 better perfromance
vis.gam(model6)

# model 7 change error familt to a truncated normal... i think

model7<-gam(waterdepth~s(jday,k=-1,bs="cc", by=as.factor(YEAR))+
              scale(CPRCP)+scale(MTEMP)+scale(MWDSP)+
              scale(CPRCP):scale(MTEMP):scale(MWDSP)+
              as.factor(area)+as.factor(dipwell), 
            family=scat(link="identity"), 
            data = foo, method = "REML",na.rm=T)
model7

#plot(model7, residuals=T) # WTF?
summary(model7) #R-sq.(adj) =  0.307   Deviance explained = 25.7%

gam.check(model7) #residuals look pretty good, qq plot poor
AIC(model7) #-5075.157 better perfromance
vis.gam(model6)

# next steps add in random terms perhaps frp nesting of dipwells within sites and also perhapd autoregressive correlation structure associated with spatial/temporal aspects (or is the s(jday?))
foo$area
d<-foo%>%filter(YEAR == "2015") %>% select(CPRCP, MTEMP, MWDSP)
summary(d)
# make a prediction from teh model
pdat<-expand.grid(jday=seq(1,352,1), YEAR="2015",CPRCP =15 , MTEMP = 10, MWDSP =5, area = "T34" )
pdat                  
pred<-predict.gam(model6,pdat,type="response", se.fit = T)

predframe<-data.frame(pdat, preds=pred); pred
plot(waterdepth~jday, data = foo[foo$YEAR == "2015",],ylim=c(-0.5,0.3))
lines(predframe$preds.fit~predframe$jday)
predframe$upperse<-(predframe$preds.fit + predframe$preds.se.fit)
predframe$lowerse<-(predframe$preds.fit - predframe$preds.se.fit)
lines(predframe$upperse~predframe$jday, lty=2)
lines(predframe$lowerse~predframe$jday, lty = 2)

# 
foo$YEAR<-as.factor(foo$YEAR)
foo$month<-as.factor(foo$month)
ggplot(data =foo)+
  geom_boxplot(aes(y=waterdepth, x=month),
               fill= "green", outlier.colour = "blue",outlier.shape = 1, notch=T)+
  facet_wrap(~YEAR)+
  theme_minimal()

