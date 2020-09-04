library(MASS)
attach(Boston)
?Boston
head(Boston)
dim(Boston)
names(Boston)
str(Boston)
nrow(Boston)
ncol(Boston)
summary(Boston)
summary(Boston$crim)

library(ISLR)
data(Auto)
head(Auto)
names(Auto)
summary(Auto)
summary(Auto$mpg)
fivenum(Auto$mpg)
boxplot(Auto$mpg)
hist(Auto$mpg)
summary(Auto$horsepower)
summary(Auto$weight)
fivenum(Auto$weight)
boxplot(Auto$weight)
mean(Auto$weight)
median((Auto$weight))

setwd("C:/Users/phoen/Dropbox/MGMT 6962")

help(read.csv)
data1<-read.csv(file.choose(), header=T, skip=1)
data1

head(data1)

summary(data1)

summary(data1$DALY_pt)
boxplot(data1$DALY_pt)
fivenum(data1$DALY_pt, na.rm=TRUE)
