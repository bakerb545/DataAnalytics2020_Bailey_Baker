rm(list=ls())

set.seed(12345)
#help(par)
par(mar=rep(.2,4))
data_Matrix<- matrix(rnorm(400), nrow=40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
#help(heatmap)
#help(rep)
par(mar=rep(.2,4))
heatmap(data_Matrix)
help(rbinom)

set.seed(678910)
for (i in 1:40){
  coin_Flip<- rbinom(1, size=1, prob=.5)
  if (coin_Flip){
    data_Matrix[i,]<- data_Matrix[i,]+rep(c(0,3), each=5)
  }
}

par(mar=rep(.2,4))
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
par(mar=rep(.2,4))
heatmap(data_Matrix)

hh<- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, , xlab="The Row Mean", 
     ylab = "Row", pch=19)
plot(colMeans(data_Matrix_Ordered), xlab="Column", ylab = "Column Mean", pch=19)

###Titanic Data
data("Titanic")
#titanicdf<- Titanic
#rpart
require(rpart)
require(C50)
require(titanic)
lis<- c("Ticket", "Cabin", "Name", "PassengerId", "Sex", "Embarked")
titanicdf<- titanic_train[,!(names(titanic_train) %in% lis)]
titanic_rpart<- rpart(Survived ~., data=titanicdf)
titanic_rpart
plot(titanic_rpart)
summary(titanic_rpart)
printcp(titanic_rpart)
plotcp(titanic_rpart)

#ctree
require(party)
titanic_ctree <- ctree(Survived~., data=titanicdf)
titanic_ctree
plot(titanic_ctree)

#hclust
tmat<- as.matrix(titanicdf)
for (i in 1:ncol(tmat)){
  tmat[,i]<- as.numeric(tmat[,i])
}
clusttitanic<- hclust(dist(tmat))
data_Matrix_titanic <- tmat[clusttitanic$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_titanic)[,nrow(data_Matrix_titanic):1])

#randomForest
library("randomForest")
rand_titanic<- randomForest(Survived~., data=titanicdf, na.action = na.omit)
print(rand_titanic)



