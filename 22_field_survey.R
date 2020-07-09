# field survey data#
df<-read.csv(file.choose())
unique(df$Common.Name)
unique(df$Binomial) # 517 species
df[df$Binomial== "Sematophyllum substrumulosum",]
b<-read.csv(file.choose())
b1<-b[b$Designated.name %in% df$Binomial,]
unique(as.factor(as.character(b1$Common.name))) # 220 species of conservation concern
c<-read.csv(file.choose())
c1<-c[c$Anotrichium.barbatum %in% df$Binomial,]
foo<-b1[b1$Designation ==  "Nationally Rare. Excludes Red Listed taxa",]
summary(as.factor(as.character(foo$Common.name)))

summary(b1$Designation)
plot(df[8], axes = T)

foo<-summary(df$vernacular)
fee<-unique(df$vernacular)