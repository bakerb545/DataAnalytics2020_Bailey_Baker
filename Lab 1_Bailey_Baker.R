rm(list=ls())

library(MASS)
library(ISLR)
EPI_data <- read.csv("C:/Users/phoen/Dropbox/MGMT 6962/EPI_data.csv")
#or
#EPI_data <- read.xlsx(â€<path>/2010EPI_data.xlsx")
# Note: replace default data frame name â€“ cannot start with numbers!
View(EPI_data)
#
attach(EPI_data) 	# sets the â€˜defaultâ€™ object
#fix(EPI_data) 	# launches a simple data editor
EPI 			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array

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


###
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
##exercise 1
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
ELPD<- EPILand[!is.na(EPIHPD)]
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






