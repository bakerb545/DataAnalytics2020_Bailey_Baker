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

###Logistic Regression
library(ISLR)
data("Smarket")
head(Smarket)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
help("glm")
glm.fit.model1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit.model1)
glm.probs<- predict(glm.fit.model1, type="response")
glm.probs[1:10]
contrasts(Direction)
help("rep")
glm.pred<- rep("Down", 1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction)
(507+145)/1250
mean(glm.pred==Direction)

train<- (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005<- Direction[!train]

glm.fit.model2<- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.prob2 = predict(glm.fit.model2, Smarket.2005, type="response")
glm.pred2<- rep("Down", 252)
glm.pred2[glm.prob2>.5]="Up"
table(glm.pred2, Direction.2005)
mean(glm.pred2==Direction.2005)
mean(glm.pred2!= Direction.2005)

glm.fit.model3<- glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.prob3 = predict(glm.fit.model3, Smarket.2005, type="response")
glm.pred3<- rep("Down", 252)
glm.pred3[glm.prob3>.5]="Up"
table(glm.pred3, Direction.2005)
mean(glm.pred3==Direction.2005)

###LDA
library(MASS)
names(iris)
dim(iris)
head(iris)
set.seed(555)
Train<- sample(1:nrow(iris), nrow(iris)/2)
iris_Train<- iris[Train,]
iris_Test<- iris[-Train,]
help(lda)
fit1<- lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris_Train)
predict1<- predict(fit1, iris_Train)
predict1_class<- predict1$class
table1<- table(predict1_class, iris_Train$Species)
table1
sum(diag(table1))/sum(table1)

