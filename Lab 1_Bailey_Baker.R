rm(list=ls())

library(MASS)
library(ISLR)
EPI_data <- read.csv("C:/Users/phoen/Dropbox/MGMT 6962/EPI_data.csv")
View(EPI_data)
#
attach(EPI_data)
#fix(EPI_data) 	# launches a simple data editor
EPI 		
tf <- is.na(EPI) 
E <- EPI[!tf]

#other data
#GRUMP_data <- read.csv(â€<path>/GPW3_GRUMP_SummaryInformation_2010.csv")

#"C:/Users/phoen/Dropbox/MGMT 6962/'
summary(EPI)

fivenum(EPI, na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI, na.rm=TRUE, bw=1.))
rug(EPI)

######
##exercise 1
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI)
qqline(EPI)
x<- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for tdsn")
qqline(x)

#first variable
plot(ecdf(WATER_E), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(WATER_E)
qqline(WATER_E)


#second variable

plot(ecdf(DALY_pt), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(DALY_pt)
qqline(DALY_pt)

#comparing dist
boxplot(EPI, DALY)
qqplot(EPI, DALY)

boxplot(EPI, ENVHEALTH)
qqplot(EPI, ENVHEALTH)

boxplot(EPI, ECOSYSTEM)
qqplot(EPI, ECOSYSTEM)

boxplot(EPI, AIR_H)
qqplot(EPI, AIR_H)

boxplot(EPI, WATER_H)
qqplot(EPI, WATER_H)

boxplot(EPI, AIR_E)
qqplot(EPI, AIR_E)

boxplot(EPI, WATER_E)
qqplot(EPI, WATER_E)

boxplot(EPI, BIODIVERSITY)
qqplot(EPI, BIODIVERSITY)

#

boxplot(ENVHEALTH, ECOSYSTEM)
qqplot(ENVHEALTH, ECOSYSTEM)

boxplot(ENVHEALTH, DALY)
qqplot(ENVHEALTH, DALY)

boxplot(ENVHEALTH, AIR_H)
qqplot(ENVHEALTH, AIR_H)

boxplot(ENVHEALTH, WATER_H)
qqplot(ENVHEALTH, WATER_H)

boxplot(ENVHEALTH, AIR_E)
qqplot(ENVHEALTH, AIR_E)

boxplot(ENVHEALTH, WATER_E)
qqplot(ENVHEALTH, WATER_E)

boxplot(ENVHEALTH, BIODIVERSITY)
qqplot(ENVHEALTH, BIODIVERSITY)
#
boxplot(ECOSYSTEM, DALY)
qqplot(ECOSYSTEM, DALY)

boxplot(ECOSYSTEM, AIR_H)
qqplot(ECOSYSTEM, AIR_H)

boxplot(ECOSYSTEM, WATER_H)
qqplot(ECOSYSTEM, WATER_H)

boxplot(ECOSYSTEM, AIR_E)
qqplot(ECOSYSTEM, AIR_E)

boxplot(ECOSYSTEM, WATER_E)
qqplot(ECOSYSTEM, WATER_E)

boxplot(ECOSYSTEM, BIODIVERSITY)
qqplot(ECOSYSTEM, BIODIVERSITY)
#
boxplot(DALY, AIR_H)
qqplot(DALY, AIR_H)

boxplot(DALY, WATER_H)
qqplot(DALY, WATER_H)

boxplot(DALY, AIR_E)
qqplot(DALY, AIR_E)

boxplot(DALY, WATER_E)
qqplot(DALY, WATER_E)

boxplot(DALY, BIODIVERSITY)
qqplot(DALY, BIODIVERSITY)

#

boxplot(AIR_H, WATER_H)
qqplot(AIR_H, WATER_H)

boxplot(AIR_H, AIR_E)
qqplot(AIR_H, AIR_E)

boxplot(AIR_H, WATER_E)
qqplot(AIR_H, WATER_E)

boxplot(AIR_H, BIODIVERSITY)
qqplot(AIR_H, BIODIVERSITY)

#

boxplot(WATER_H, AIR_E)
qqplot(WATER_H, AIR_E)

boxplot(WATER_H, WATER_E)
qqplot(WATER_H, WATER_E)

boxplot(WATER_H, BIODIVERSITY)
qqplot(WATER_H, BIODIVERSITY)

#

boxplot(AIR_E, WATER_E)
qqplot(AIR_E, WATER_E)

boxplot(AIR_E, BIODIVERSITY)
qqplot(AIR_E, BIODIVERSITY)

#

boxplot(WATER_E, BIODIVERSITY)
qqplot(WATER_E, BIODIVERSITY)


### Exercise 2
EPILand<- EPI[!Landlock]
ELand<- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)
summary(ELand)

fivenum(ELand, na.rm=TRUE)
stem(ELand)
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)
lines(density(ELand, na.rm=TRUE, bw=1.))
rug(ELand)

######
plot(ecdf(ELand), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(ELand)
qqline(ELand)

#No_surface_water
EPISW<- EPI[!No_surface_water]
ESW<- EPILand[!is.na(EPISW)]
hist(ESW)
hist(ESW, seq(30., 95., 1.0), prob=TRUE)
summary(ESW)

fivenum(ESW, na.rm=TRUE)
stem(ESW)
hist(ESW)
hist(ESW, seq(30., 95., 1.0), prob=TRUE)
lines(density(ESW, na.rm=TRUE, bw=1.))
rug(ESW)

plot(ecdf(No_surface_water), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(No_surface_water)
qqline(No_surface_water)

#Desert
EPIND<- EPI[!Desert]
END<- EPILand[!is.na(EPIND)]
hist(END)
hist(END, seq(30., 95., 1.0), prob=TRUE)
summary(END)

fivenum(END, na.rm=TRUE)
stem(END)
hist(END)
hist(END, seq(30., 95., 1.0), prob=TRUE)
lines(density(END, na.rm=TRUE, bw=1.))
rug(END)

plot(ecdf(Desert), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(Desert)
qqline(Desert)

#High_Population_Density
EPIHPD<- EPI[!High_Population_Density]
ELPD<- EPIHPD[!is.na(EPIHPD)]
hist(ELPD)
hist(ELPD, seq(30., 95., 1.0), prob=TRUE)
summary(ELPD)

fivenum(ELPD, na.rm=TRUE)
stem(ELPD)
hist(ELPD)
hist(ELPD, seq(30., 95., 1.0), prob=TRUE)
lines(density(ELPD, na.rm=TRUE, bw=1.))
rug(ELPD)

plot(ecdf(High_Population_Density), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(High_Population_Density)
qqline(High_Population_Density)

###filtering on EPI_regions
EPI_Europe<- EPI_data[EPI_data$EPI_regions=="Europe",]
View(EPI_Europe)

#filtering on GEO_subregion
EPI_Carib<- EPI_data[EPI_data$GEO_subregion=="Caribbean",]
View(EPI_Carib)


####
###
####


###GPW3_Grump
GPW_data <- read.csv("C:/Users/phoen/Dropbox/MGMT 6962/GPW3_GRUMP_SummaryInformation_2010.csv")
View(GPW_data)
#
attach(GPW_data)
#fix(EPI_data) 	# launches a simple data editor
PPU<- PopulationPerUnit		
PPU
tf <- is.na(PPU) 
E <- PPU[!tf]

summary(PPU)

fivenum(PPU, na.rm=TRUE)
stem(PPU)
hist(PPU)
hist(PPU, seq(0., 3000., 10.0), prob=TRUE)
lines(density(PPU, na.rm=TRUE, bw=1.))
rug(PPU)

######
##exercise 1
plot(ecdf(PPU), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(PPU)
qqline(PPU)
x<- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for tdsn")
qqline(x)

#first variable
Area<- as.numeric(Area)
plot(ecdf(Area), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(Area)
qqline(Area)

#second variable
NumUnits<- as.numeric(NumUnits)
plot(ecdf(NumUnits), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(NumUnits)
qqline(NumUnits)

#comparing dist
boxplot(PPU, Area)
qqplot(PPU, Area)

boxplot(PPU, NumUnits)
qqplot(PPU, NumUnits)

boxplot(Area, NumUnits)
qqplot(Area, NumUnits)

##exercise 2
GPWArea<- Area[Area<=50000]
GPWA<- GPWArea[!is.na(GPWArea)]
hist(GPWA)
hist(GPWA, seq(0., 50000., 100.0), prob=TRUE)
summary(GPWA)

fivenum(GPWA, na.rm=TRUE)
stem(GPWA)
hist(GPWA)
hist(GPWA, seq(0., 50000., 100.0), prob=TRUE)
lines(density(GPWA, na.rm=TRUE, bw=100.))
rug(GPWA)

######
plot(ecdf(GPWA), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(GPWA)
qqline(GPWA)


##filtering
GPWEur<- GPW_data[GPW_data$ContinentName=="Europe",]
View(GPWEur)

###
#####
###

#Water-treatment
water_data <- read.csv("C:/Users/phoen/Dropbox/MGMT 6962/water-treatment.csv")
View(water_data)
#
attach(water_data)
#fix(EPI_data) 	# launches a simple data editor
Q.E<- as.numeric(Q.E)
tf <- is.na(Q.E) 
E <- PPU[!tf]

summary(Q.E)

fivenum(Q.E, na.rm=TRUE)
stem(Q.E)
hist(Q.E)
hist(PPU, seq(0., 65000., 1000.0), prob=TRUE)
lines(density(Q.E, na.rm=TRUE, bw=1000.))
rug(Q.E)

######
##exercise 1
plot(ecdf(Q.E), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(Q.E)
qqline(Q.E)

#first variable
ZN.E<- as.numeric(ZN.E)
plot(ecdf(ZN.E), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(ZN.E)
qqline(ZN.E)

#second variable
PH.E<- as.numeric(PH.E)
plot(ecdf(PH.E), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(PH.E)
qqline(PH.E)

#comparing dist
boxplot(Q.E, ZN.E)
qqplot(Q.E, ZN.E)

boxplot(Q.E, PH.E)
qqplot(Q.E, PH.E)

boxplot(ZN.E, PH.E)
qqplot(ZN.E, PH.E)

##exercise 2
waterQ<- Q.E[Q.E<=40000]
WQ<- waterQ[!is.na(waterQ)]
hist(WQ)
hist(WQ, seq(0., 40000., 1000.0), prob=TRUE)
summary(WQ)

fivenum(WQ, na.rm=TRUE)
stem(WQ)
hist(WQ)
hist(WQ, seq(0., 50000., 1000.0), prob=TRUE)
lines(density(WQ, na.rm=TRUE, bw=1000.))
rug(WQ)

######
plot(ecdf(WQ), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(WQ)
qqline(WQ)


##filtering
waterDBO<- water_data[water_data$DBO.E<= 250,]
View(waterDBO)


