rm(list=ls())

###Lab1
#bronx1
library(gdata)
library(readxl)
bronx1<-read_xls("C:/Users/phoen/Downloads/rollingsales_bronx.xls", sheet=1,skip=4) 

View(bronx1)
attach(bronx1)
`SALE\nPRICE`<- sub("\\$", "", `SALE\nPRICE`)
`SALE\nPRICE`<- as.numeric(gsub(",", "", `SALE\nPRICE`))
`GROSS SQUARE FEET`<-as.numeric(gsub(",","", `GROSS SQUARE FEET`)) 
`LAND SQUARE FEET`<-as.numeric(gsub(",","", `LAND SQUARE FEET`))
bronx1<-bronx1[which(bronx1$`GROSS SQUARE FEET`!=0 & bronx1$`LAND SQUARE FEET`!=0 & bronx1$`SALE\nPRICE`!=0),]
plot(log(`GROSS SQUARE FEET`), log(`SALE\nPRICE`)) 
m1<-lm(log(`SALE\nPRICE`)~log(`GROSS SQUARE FEET`))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

m2<-lm(log(bronx1$SALE.PRICE)~log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

m3<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

m4<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))

###Lab2




###Lab3






