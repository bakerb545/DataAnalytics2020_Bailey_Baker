rm(list=ls())

url<- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heartdata<- read.csv(url, header=FALSE)
head(heartdata)
colnames(heartdata)<- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
                        "thalach", "exang", "oldpeak", "slope", "thal", "hd")
head(heartdata)
str(heartdata)

heartdata[heartdata=="?"]<- NA
heartdata[heartdata$sex==0,]$sex<- "F"
heartdata[heartdata$sex==1,]$sex<- "M"
heartdata$sex<- as.factor(heartdata$sex)
heartdata$cp<- as.factor(heartdata$cp)
heartdata$fbs<- as.factor(heartdata$fbs)
heartdata$restecg<- as.factor(heartdata$restecg)
heartdata$exang<- as.factor(heartdata$exang)
heartdata$slope<- as.factor(heartdata$slope)
heartdata$cp<- as.integer(heartdata$cp)
heartdata$cp<- as.factor(heartdata$cp)
heartdata$thal<- as.integer(heartdata$thal)
heartdata$thal<- as.factor(heartdata$thal)
heartdata$hd<- ifelse(test=heartdata$hd==0, yes="Healthy", no="Unhealthy")
heartdata$hd<- as.factor(heartdata$hd)
str(heartdata)

nrow(heartdata[is.na(heartdata$cp) | is.na(heartdata$thal),])
heartdata[is.na(heartdata$cp)| is.na(heartdata$thal),]
nrow(heartdata)
heartdata<- heartdata[!(is.na(heartdata$cp)| is.na(heartdata$thal)),]
nrow(heartdata)

xtabs(~heartdata$hd + heartdata$sex, data=heartdata)
xtabs(~heartdata$hd + heartdata$cp, data=heartdata)
xtabs(~ heartdata$hd + heartdata$fbs, data=heartdata)
xtabs(~ heartdata$hd + heartdata$restecg, data=heartdata)

logis<- glm(heartdata$hd~heartdata$sex, data=heartdata, family="binomial")
summary(logis)

###multiple model selection
library(caret)
data(iris)
dataset<- iris
head(dataset)
validation_index<- createDataPartition(dataset$Species, p=.8, list=FALSE)
validation<- dataset[-validation_index,]
dataset<- dataset[validation_index,]
dim(dataset)
sapply(dataset,class)
head(dataset)
levels(dataset$Species)
percentage<- prop.table(table(dataset$Species))*100
cbind(freq=table(dataset$Species), percentage=percentage)
summary(dataset)
x<- dataset[,1:4]
y<- dataset[,5]
par(mfrow=c(1,4))
for (i in 1:4){
  boxplot(x[,i], main=names(iris[i]))
}
plot(y)
featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")
scales<- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

control<-trainControl(method="cv", number=10)
metric<- "Accuracy"
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)
print(fit.lda)
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)

#oats, lme
library(MASS)
library(nlme)
library(lme4)
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


