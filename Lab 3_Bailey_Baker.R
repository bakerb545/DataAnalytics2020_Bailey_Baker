rm(list=ls())

library(rpart)
library(rpart.plot)
library(ggplot2)

data("msleep")
str(msleep)
help(msleep)
str(data)

mSleepDF1<- msleep[,c(3,6,10,11)]
str(mSleepDF1)
head(mSleepDF1)

#help(rpart)
sleepModel_1<- rpart(sleep_total~., data=mSleepDF1, method="anova")
sleepModel_1

help(rpart.plot)
rpart.plot(sleepModel_1, type=3, fallen.leaves = TRUE)
rpart.plot(sleepModel_1, type=3, digits=3, fallen.leaves = TRUE)
rpart.plot(sleepModel_1, type=3, digits=4, fallen.leaves = TRUE)

#install.packages("C50")
library(C50)
require(C50)
data("iris")
head(iris)
str(iris)
table(iris$Species)

set.seed(9850)
grn<- runif(nrow(iris))

irisrand<- iris[order(grn),]
str(irisrand)
classificationmodel1<- C50(irisrand[1:100, -5], irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)

prediction1<- predict(classificationmodel1, irisrand[101:150,])
prediction1

table(irisrand[101:150,5], prediction1)
plot(classificationmodel1)

library("e1071")
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,5]), iris[,5], dnn=list('predicted', 'actual'))
classifer$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, .1736640),0,8,col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, .4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, .5518947), add=TRUE, col="green")

