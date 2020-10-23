rm(list=ls())
###Titanic Data
data("Titanic")
#titanicdf<- Titanic
#rpart
require(rpart)
require(C50)
require(titanic)
require(fastDummies)

par(mfrow=c(1,2)) 
lis<- c("Ticket","Name", "PassengerId" ,"Cabin")
titanicdf<- titanic_train[,!(names(titanic_train) %in% lis)]
#titanicdf<- titanic_train
titanic_rpart<- rpart(Survived ~., data=titanicdf)
titanic_rpart
plot(titanic_rpart, uniform=TRUE)
text(titanic_rpart, use.n=TRUE, all=TRUE, cex=.6)
summary(titanic_rpart)
printcp(titanic_rpart)
plotcp(titanic_rpart)

#ctree
require(party)
titanicdf <- dummy_cols(.data = titanicdf, select_columns = c("Sex", "Embarked"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)
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
heatmap(tmat)
