# pyc_precip data
# 22 10 2019
# Mike Shewring

library(lubridate)
pyc_prcp<-read.csv(file.choose())
head(pyc_prcp)
pyc_prcp$date1<-dmy(pyc_prcp$date)
pyc_prcp$month<-month(pyc_prcp$date1)
pyc_prcp$jday<-yday(pyc_prcp$date1)
pyc_prcp$YEAR<-year(pyc_prcp$date1)

pyc_prcp$YEAR<-as.factor(pyc_prcp$YEAR)
pyc_prcp$month<-as.factor(pyc_prcp$month)

PyC_month_sum_precip1 <- pyc_prcp %>%
  group_by(month, YEAR) %>% # group by the day column
  summarise(CPRCP=sum(rain)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()

ggplot(data = PyC_month_sum_precip1)+
  geom_col(aes(y=CPRCP, x=month),
           fill= "blue", )+
  facet_wrap(~YEAR)+
  theme_minimal()

# cumulative precipitation by month 
ggplot(data = PyC_month_sum_precip1)+
  geom_col(aes(y=CPRCP, x=month),
           fill= "blue", )
theme_minimal()