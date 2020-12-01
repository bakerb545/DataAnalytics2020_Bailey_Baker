rm(list=ls())
require(datasets)

data(economics, package="ggplot2") 
economics$index<- 1:nrow(economics)
economics<- economics[1:80,]
loessMod10<- loess(uempmed~index, data=economics, span = .1)
loessMod25<- loess(uempmed~index, data=economics, span = .25)
loessMod50<- loess(uempmed~index, data=economics, span = .5)

smoothed10<-predict(loessMod10) 
smoothed25<-predict(loessMod25) 
smoothed50<-predict(loessMod50) 

plot(economics$uempmed, x=economics$date, type="l",
     main="Loess Smoothing and Prediction", xlab="Date",
     ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")

data("cars")
str(cars)
plot(speed~dist, data=cars)

lowess(cars$speed ~ cars$dist)
lines(lowess(cars$speed~cars$dist, f=2/3), col="blue")
lines(lowess(cars$speed~cars$dist, f=.8), col="red")
lines(lowess(cars$speed~cars$dist, f=.9), col="green")
lines(lowess(cars$speed~cars$dist, f=.1), col=5)
lines(lowess(cars$speed~cars$dist, f=.01), col=6)
