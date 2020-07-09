# dipwell data modelling
library(GSODR)
library(tidyverse)
library(lubridate)
# preciptation data-
tbar<-nearest_stations(51.706771,-3.5876356, 50)
tdat<-get_GSOD(years = 2014:2018, tbar)
tdat<-tdat%>%filter(STN_NAME =="SENNYBRIDGE NO2")%>%select(YEAR, MONTH, DAY, YDAY, PRCP)
tdat$date<-dmy(paste0(tdat$DAY,tdat$MONTH, tdat$YEAR))
tdat$date1<-tdat$date
head(tdat)
tdat$date2<-foo$date1
write.csv(tdat, "E:/PyC/PRCPData.csv")

# dipwell data
list.files("E:\\PyC\\dipwell_data", pattern = "csv")
dip<-read.csv(file.choose())
head(dip)
tail(dip)
dip$dipwell
dip$date
dip1<-gather(dip, key = dipwell, value = date  )
dip1$date1<-rep(dip$date,12)
dip1$waterdepth<-dip1$date
dip1$date<-NULL
rm(dip)
library(lubridate)
dip1$date1<-dmy(dip1$date1)
dip1$month<-as.factor(month(dip1$date1))
dip1$jday<-yday(dip1$date1)
dip1$year<-year(dip1$date1)
summary(dip1$year)

# merge datasets
?left_join
foo<-left_join(dip1, tdat, by = "date1")
foo$date2<-foo$date1 -3
foo

# merge datasets using culmulative sum or precipitation between times
lubridate::week()
week_sum_precip <- tdat %>%
  mutate(week = lubridate::week(date1)) %>%
  group_by(week, YEAR) %>% # group by the day column
  summarise(CPRCP=sum(PRCP)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()
?left_join
foo$week<-week(foo$date1)
foo$YEAR<-as.character(foo$year)
foo<-left_join(foo, week_sum_precip, by = c("week","YEAR"))
head(foo)
write.csv()
write.csv(foo, file="E:/PyC/T34DipWellData.csv")
# iterartor that does not work... # check thishttps://community.rstudio.com/t/iterate-over-rows-to-conditionally-calculate-cumulative-sum/6016/2
#cprcp <- function(df, tdat1) {
  dates <- df[["date1"]]
  res <- purrr::map_int(dates, function(date) {
    first_date<-date
    sc_date<-date-3
      tdat %>% dplyr::filter(date1 > sc_date & date1 < first_date) %>% 
      dplyr::pull(PRCP) %>%
     sum(na.rm = T)
  })
  df[["CPRCP"]] <- res
  df
  }

plot(dip1$waterdepth~dip1$date1)
plot(dip1$waterdepth~dip1$month)

library(ggplot2)
ggplot(data=dip1)+
  geom_point(aes(x=date1, y = waterdepth))+
  geom_smooth(aes(x=date1, y = waterdepth))


library(mgcv)

# model1  day only
model1<-gam(waterdepth~s(jday, k=-1)+as.factor(year), data = foo)
model1

plot.gam(model1)

summary(model1) #R-sq.(adj) =  0.328   Deviance explained = 35.2% GCV score: 0.004829164
gam.check(model1) #looks pretty good qq plot could be better
AIC(model1)  -1982.264

# model 2 - including PRCP
model2<-gam(waterdepth~s(jday,k=-1)+PRCP+as.factor(year), data = foo, na.rm=T)
model2

plot(model2)
summary(model2) # R-sq.(adj) =  0.328   Deviance explained = 35.3% GCV = 0.0043202

gam.check(model2) #looks pretty good qq plot could be better
AIC(model2) -1998.327

# model 3 includeing cumulative
model3<-gam(waterdepth~s(jday)+PRCP+CPRCP+as.factor(year), data = foo,na.rm=T)
model3

plot(model3)
summary(model3) # R-sq.(adj) =  0.356   Deviance explained = 38.9% GCV = 0.0042008

gam.check(model3) #looks pretty good qq plot could be better
AIC(model3) -1996.025

# model 4 includeing cumulative

model4<-gam(waterdepth~s(jday)+CPRCP, data = foo, method = "REML",na.rm=T)
model4
gam.check(model4)

plot(model4, residuals=T)
summary(model4) # R-sq.(adj) =  0.407   Deviance explained = 43.6% GCV = 0.0043236

gam.check(model4) #looks pretty good qq plot could be better
AIC(model4) -4748.638

# model 5 includeing cumulative

model5<-gam(waterdepth~s(jday)+CPRCP+as.factor(year), data = foo, method = "REML",na.rm=T)
model5
gam.check(model5)

plot(model5, residuals=T)
summary(model5) # R-sq.(adj) =  0.407   Deviance explained = 43.6% GCV = 0.0043236

gam.check(model5) #looks pretty good qq plot could be better
AIC(model5) 

# check the fit
fitted.gam<-model4$fitted.values
residuals<-model4$residuals
sresid<-(residuals - mean(residuals)/sd(residuals))
hist(sresid)
plot(fitted.gam~sresid)
plot(foo$waterdepth[1:266]~fitted.gam)
mean(foo$CPRCP)
d<-foo%>%filter(YEAR == "2014") %>% select(CPRCP)
summary(d)
# make a prediction from teh model
pdat<-expand.grid(jday=seq(1,352,1), CPRCP =16)
pdat                  
pred<-predict.gam(model4,pdat,type="response", se.fit = T)
predframe<-data.frame(pdat, preds=pred); pred

#plot(waterdepth~jday, data = foo[foo$YEAR == "2018",],ylim=c(-0.4,0))
plot(waterdepth~jday, data = foo,ylim=c(-0.4,0))
lines(predframe$preds.fit~predframe$jday)
predframe$upperse<-(predframe$preds.fit + predframe$preds.se.fit)
predframe$lowerse<-(predframe$preds.fit - predframe$preds.se.fit)
lines(predframe$upperse~predframe$jday, lty=2)
lines(predframe$lowerse~predframe$jday, lty = 2)
