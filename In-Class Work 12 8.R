rm(list=ls())

library(lme4)
library(MASS)

politeness<-
  read.csv("http://www.bodowinter.com/uploads/1/2/9/3/1293
62560/politeness_data.csv")
politeness


which(is.na(politeness$frequency))

boxplot(frequency~attitude*gender, col=c("white", "lightgray"),
        politeness)

politeness.model = lmer(frequency~attitude+
                          (1|subject)+(1|scenario), data=politeness)
summary(politeness.model)

library(MASS)
library(nlme)
data(oats)
names(oats) = c('block', 'variety', 'nitrogen', 'yield')
oats$mainplot = oats$variety
oats$subplot = oats$nitrogen
summary(oats)
library(nlme)
m1.nlme = lme(yield ~ variety*nitrogen,
              random = ~ 1|block/mainplot,
              data = oats)
summary(m1.nlme)
anova(m1.nlme)

#fitting regression tree
library(MASS)
library(tree)
set.seed(1)
help("Boston")
head(Boston)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~., Boston, subset=train)
summary(tree.boston)

tree(formula=medv~., data=Boston, subset=train)
plot(tree.boston)
text(tree.boston, pretty=0)

help("cv.tree")
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, typ='b')

help("prune.tree")
prune.boston = prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)
yhat = predict(tree.boston, newdata=Boston[-train,])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

#bagging and randomforest
library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, importance=T)
bag.boston

yhat.bag=predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25,importance=T)
yhat.bag=predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

set.seed(1)
rf.boston=randomForest(medv~., data=Boston, subset=train,
                       mtry=6, importance=T)
yhat.rf = predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

importance(rf.boston)
varImpPlot(rf.boston)

library(caret)
data(iris)
dataset<- iris
head(dataset)

validation_index<- createDataPartition(dataset$Species, p=.8, list=F)
validation<- dataset[-validation_index,]
dataset<- dataset[validation_index,]
dim(dataset)

sapply(dataset, class)
head(dataset)
levels(dataset$Species)
percentage<- prop.table(table(dataset$Species))*100
cbind(freq=table(dataset$Species), percentage=percentage)

summary(dataset)
x<- dataset[,1:4]
y<- dataset[,5]
par(mfrow=c(1,4))
for (i in 1:4){
  boxplot(x[,i], main=names(iris)[i])
}

plot(y)
featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")
scales<- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

control<- trainControl(method="cv", number=10)
metric<- "Accuracy"
#build models
set.seed(7)
fit.lda<- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
set.seed(7)
fit.cart<- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
set.seed(7)
fit.knn<- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
set.seed(7)
fit.svm<- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
set.seed(7)
fit.rf<- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

results<- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

dotplot(results)
print(fit.lda)
predictions<- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
