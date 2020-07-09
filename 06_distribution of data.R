rlnorm(n, meanlog = -0.1, sdlog=0.2)
m<-dlnorm(x, meanlog = 0, sdlog = 1, log = FALSE)
?dlnorm
#install.packages("fitdistrplus")
library(fitdistrplus)
e<-foo[!is.na(foo$waterdepth),]
e$waterdepth<-e$waterdepth*-1
hist(e$waterdepth)
shapiro.test(e$waterdepth) # not normaly distributed

#e$waterdepth[e$waterdepth<=0] = 0.00000000001
plotdist(e$waterdepth, histo = TRUE, demp = TRUE)
descdist(e$waterdepth, boot = 10000)
hist(e$waterdepth)
plot(e$waterdepth~as.factor(e$month))
e$YEAR<-as.factor(e$YEAR)
?gam
# model fitting continued
model9<-gam(waterdepth~s(jday, k=-1, bs="cc", by = YEAR)+CPRCP+MTEMP, 
            family=scat(link="identity") ,data = e, method = "REML",
            na.rm=T)
model9

plot(model9)

summary(model9) # R-sq.(adj) =  0.257   Deviance explained = 25.2%

gam.check(model9) #residuals look pretty good, qq plot poor
AIC(model9)   # -2905.371

e$waterdepth
# model  11
model11<-gam(waterdepth~s(jday, k=-1, bs="cc", by = YEAR)+CPRCP+MTEMP+area, 
                 family=scat(link="identity") ,data = e, method = "REML",
                 na.rm=T)
model11

plot(model11, residuals=T)

summary(model11)  #R-sq.(adj) =  0.284   Deviance explained = 27.5%

gam.check(model11) # ok
AIC(model11)   -2969.455

# model  12
e$dipwell<-as.factor(e$dipwell)
e$area<-as.factor(e$area)
model12<-gamm(waterdepth~s(jday, k=-1, bs="cc", by = YEAR)+CPRCP+MTEMP+area,
              random = list(dipwell = ~1), 
              family=gaussian(link=identity), data = e, method = "REML")
model12

plot(model12$gam, residuals=T)
plot(model12$lme, residuals=T)

summary(model12$gam) # 
summary(model12$lme)
gam.check(model12$gam) 
AIC(model12)   
