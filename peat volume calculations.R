# peat volume calculations
library(raster)
creg<-raster(file.choose())
creg
plot(creg)
?freq
f<-freq(creg, digits=1)
sum(f[6:30,])*100
3893550/10000

creg
pd<-creg[]
pd<-pd[!is.na(pd)]
pd1<-pd[pd<= 0.25]
pd2<-pd[pd>= 0.25]
pd3<-sum(pd1)*100
peatc<-pd3*18.63
1888.1075 * 100
peatc1<-188810.8 * 18.63
# https://www.iucn-uk-peatlandprogramme.org/sites/default/files/2019-07/Peatbogs_and_carbon.pdf
# haplotelmic peatcontains 18.63kg carbon per cubic metre of peat
# catotelmic peat contains 41.88kg carbon per cubic metre of peat
sum(pd2)
pd4<-sum(pd2)*100
peatcc<-pd4*41.88
totalCaetNosC<-peatc+peatcc
PeatmtCpyc<-totalCaetNosC*0.001 # convert to metric tons
PeatmtCpyc

39946.76*100
peatc2<-3994676*41.88
totalPyC<-peatc1+peatc2
PeatmtCpyc<-totalPyC*0.001 # convert to metric tons
PeatmtCpyc


