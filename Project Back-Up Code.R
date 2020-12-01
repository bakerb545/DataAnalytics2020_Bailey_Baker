rm(list=ls())

set.seed(1)
library(readxl)
library(rpart)
library(rpart.plot)
library(zoo)
library(tree)
library(class)

confmat<- function(table){
  tp = table[4]
  fn = table[3]
  fp = table[2]
  tn = table[1]
  total_recs<- sum(tp, tn, fn, fp)
  recall_rate<- round(tp/(tp+fn),5)
  precision_rate<- round(tp/(tp+fp),5)
  accuracy_rate<- round((tp+tn)/total_recs,5)
  misclassification_rate<- round((fp+fn)/total_recs,5)
  lis<- c(as.numeric(recall_rate), as.numeric(precision_rate), as.numeric(accuracy_rate), as.numeric(misclassification_rate))
  return(lis)
}

models<- function(train, mort){
  summstats<- as.data.frame(matrix(c(NA), ncol=5))
  name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
  names(summstats)<- name
  #decision trees (classification) - rpart
  mo<- rpart(train$ABOVE25_MORT ~., data=train, method="class")
  #mo<- tree(as.factor(ABOVE25_MORT) ~., data=train)
  mo
  
  #deviance is a bit better than gini - why is that?
  dev.off()
  #par(mar=c(1,1,1,1))
  #rpart.plot(mo)
  plot(mo, uniform=TRUE)
  text(mo, pretty=0)
  
  dcpredict<- predict(mo, newdata = mort, type="class")
  dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])
  
  mat<- confmat(dctab)
  stats<- as.data.frame(matrix(c("Decision Tree", mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)
  summstats<- summstats[complete.cases(summstats),]
  summstats
  #pruning
  min_cp<- mo$cptable[which.min(mo$cptable[,"xerror"]), "CP"]
  tree_prune<- prune(mo, cp =min_cp)
  #cv_prune<-cv.tree(mo, FUN = prune.misclass)
  #cv_prune
  #tree_prune<- prune.misclass(mo, best=20)
  #rpart.plot(tree_prune)
  plot(tree_prune)
  text(tree_prune, pretty=0)
  
  dcpredict<- predict(tree_prune, newdata = mort, type=c("class"))
  dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])
  
  mat<- confmat(dctab)
  #stats<- as.data.frame(matrix(c("Decision Tree (Pruned, cp=.02)",  mat[1], mat[2], 
  #    mat[3], mat[4]), ncol=5))
  dc_prune<- paste("Decision Tree (Pruned, CP = ", round(min_cp, digits=5), ")", sep="", collapse=NULL)
  
  stats<- as.data.frame(matrix(c(dc_prune,  mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)
  summstats
  #stopping criteria
  mo_stop<- rpart(train$ABOVE25_MORT ~., data=train, method="class", control = rpart.control(minsplit=5))
  #mo_stop<- tree(as.factor(ABOVE25_MORT) ~., data=train, split=c("gini"), control = tree.control(nrow(train), mincut= 5))
  mo_stop
  #rpart.plot(mo_stop)
  plot(mo_stop)
  
  dcpredict<- predict(mo_stop, newdata = mort, type=c("class"))
  dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])
  
  mat<- confmat(dctab)
  stats<- as.data.frame(matrix(c("Decision Tree (Minsplit = 5)",  mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)
  summstats
  
  
  #random forest
  require(randomForest)
  train$ABOVE25_MORT<- as.factor(train$ABOVE25_MORT)
  for (i in 1:ncol(train)){
    train[is.na(train[,i]),i] <- mean(train[,i], na.rm=TRUE)
    mort[is.na(mort[,i]),i] <- mean(mort[,i], na.rm=TRUE)
  }
  rand_train<- randomForest(train$ABOVE25_MORT ~., data=train, na.action = na.omit, importance=TRUE)
  print(rand_train)
  predTrain<- predict(rand_train, mort, type="class")
  randtab<- table(predTrain, mort$ABOVE25_MORT)
  
  mat<- confmat(randtab)
  
  stats<- as.data.frame(matrix(c("Random Forest(ntree=500)",  mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)
  
  knnstats<- as.data.frame(matrix(c(NA), ncol=5))
  name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
  names(knnstats)<- name
  
  for (i in 1:10){
    knnmodel<- knn(train[,-which(names(train) %in% drop2)], mort[,-which(names(mort) %in% drop2)], train$ABOVE25_MORT, k=i)
    
    knntab <- table(knnmodel,mort$ABOVE25_MORT)
    
    #print(str(i))
    mat<- confmat(knntab)
    knntype<- paste("KNN (", i, ")", sep="", collapse=NULL)
    
    stats<- as.data.frame(matrix(c(knntype, mat[1], mat[2], 
                                   mat[3], mat[4]), ncol=5))
    
    names(stats)<- name
    knnstats<- rbind(knnstats, stats)
  }
  
  max_acc_knn<- knnstats[order(knnstats$Accuracy, decreasing=T),]
  summstats<- rbind(summstats, max_acc_knn[1,])
  
  lis<- list("Statistics" = summstats, "DecTree"= mo, "Pruned DecTree"= tree_prune, "Stopped DecTree"= mo_stop,
             "RF"= rand_train, "KNN stats"= max_acc_knn)
  return(lis)
  
}

mort<- data.frame(read_xlsx("C:/Users/phoen/Downloads/2017 Under-Five Mortality.xlsx"))
#View(mort)

for (i in 3:ncol(mort)-1){
  mort[,i]<- as.numeric(mort[,i])
}
summary(mort)
track<- data.frame("Name" = NA, "NAs" = NA)
mort[, "POVERTY"]<- sub("-", NA, mort[,"POVERTY"])
mort<- mort[is.na(mort$UNDER5_MORT) == FALSE,]

mort<- mort[, which(colMeans(!is.na(mort)) > 0.5)]
for (i in 1:ncol(mort)){
  track1<- data.frame("Name" = names(mort)[i], 
                      "NAs" =sum(!complete.cases(mort[,i])))
  track<- rbind(track, track1)
  
}

track<- track[complete.cases(track),]
track
track["Percent"]<- track[, "NAs"]/nrow(mort)
track<- track[order(track$Percent, decreasing=TRUE),]
track

mort_rate<- mort[,'UNDER5_MORT']
hist(mort_rate)
#unimprov_water<- mort[,'UNIMPROVED_DRINK']
print(mean(mort_rate))
mort[, 'ABOVE25_MORT']<- 0
mort[which(mort$UNDER5_MORT>= 25), 'ABOVE25_MORT'] <- 1

#boxplot(unimprov_water)
#qqnorm(unimprov_water)
summary(mort_rate)
lis<- list("Indicator", "INF_MORT", "CHILD_MORT14", "UNDER5_DEATHS", "CHILD_DEATHS14", "POVERTY", "NO_WASTE_OVERWEIGHT_STUNT", "WASTE_OVERWEIGHT_STUNT")
mort<- mort[,-which(names(mort) %in% lis)]
print(mean(mort$IMPROVED_DRINK_PREMISES, na.rm=T))
mort<- na.aggregate(mort)
print(mean(mort$IMPROVED_DRINK_PREMISES, na.rm=T))
#high_mort<- mort[(mort$UNDER5_MORT >= mean(mort_rate, na.rm=TRUE)),]
#low_mort<- mort[(mort$UNDER5_MORT < mean(mort_rate, na.rm=TRUE)),]
#summary(high_mort$UNDER5_MORT)
#hist(high_mort$UNDER5_MORT)
#summary(low_mort$UNDER5_MORT)
#hist(low_mort$UNDER5_MORT)

#unimprov_water_low<- low_mort$UNIMPROVED_DRINK
#unimprov_water_high<- high_mort$UNIMPROVED_DRINK

#summary(unimprov_water_high)
#summary(unimprov_water_low)

#import of training data
t2016<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=1))
t2015<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=3))
t2014<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=4))
t2013<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=5))
t2012<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=6))
t2011<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=7))


train<- rbind(t2016, rbind(t2015, rbind(t2014, rbind(t2013, rbind(t2012, t2011)))))

for (i in 1:ncol(train)){
  train[, i]<- sub("-", NA, train[,i])
  train[,i]<- as.numeric(train[,i])
}
train<- train[is.na(train$UNDER5_MORT) == FALSE,]
train<- train[,-which(names(train) %in% lis)]
train<- train[,which(names(train) %in% names(mort))]

train[, 'ABOVE25_MORT']<- 0
train[which(train$UNDER5_MORT>= 25), 'ABOVE25_MORT'] <- 1

summary(train)

summstats<- as.data.frame(matrix(c(NA), ncol=5))
name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
names(summstats)<- name

drop<- c("UNDER5_MORT")
drop2<- c("ABOVE25_MORT")
u5_train<- as.data.frame(train[,"UNDER5_MORT"])
u5_mort<- as.data.frame(mort[,"UNDER5_MORT"])
names(u5_train)<- c("UNDER5_MORT")
names(u5_mort)<- c("UNDER5_MORT")
train<- train[,-which(names(train) %in% drop)]
mort<- mort[,-which(names(mort) %in% drop)]

#decision trees (classification) - rpart
mo<- rpart(train$ABOVE25_MORT ~., data=train, method="class")
#mo<- tree(as.factor(ABOVE25_MORT) ~., data=train)
mo

#deviance is a bit better than gini - why is that?
dev.off()
#par(mar=c(1,1,1,1))
#rpart.plot(mo)
plot(mo, uniform=TRUE)
text(mo, pretty=0)

dcpredict<- predict(mo, newdata = mort, type="class")
dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
stats<- as.data.frame(matrix(c("Decision Tree", mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
summstats<- rbind(summstats, stats)
summstats<- summstats[complete.cases(summstats),]
summstats
#pruning
min_cp<- mo$cptable[which.min(mo$cptable[,"xerror"]), "CP"]
tree_prune<- prune(mo, cp =min_cp)
#cv_prune<-cv.tree(mo, FUN = prune.misclass)
#cv_prune
#tree_prune<- prune.misclass(mo, best=20)
#rpart.plot(tree_prune)
plot(tree_prune)
text(tree_prune, pretty=0)

dcpredict<- predict(tree_prune, newdata = mort, type=c("class"))
dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
#stats<- as.data.frame(matrix(c("Decision Tree (Pruned, cp=.02)",  mat[1], mat[2], 
#    mat[3], mat[4]), ncol=5))
dc_prune<- paste("Decision Tree (Pruned, CP = ", round(min_cp, digits=5), ")", sep="", collapse=NULL)

stats<- as.data.frame(matrix(c(dc_prune,  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
summstats<- rbind(summstats, stats)
summstats
#stopping criteria
mo_stop<- rpart(train$ABOVE25_MORT ~., data=train, method="class", control = rpart.control(minsplit=5))
#mo_stop<- tree(as.factor(ABOVE25_MORT) ~., data=train, split=c("gini"), control = tree.control(nrow(train), mincut= 5))
mo_stop
#rpart.plot(mo_stop)
plot(mo_stop)

dcpredict<- predict(mo_stop, newdata = mort, type=c("class"))
dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
stats<- as.data.frame(matrix(c("Decision Tree (Minsplit = 5)",  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
summstats<- rbind(summstats, stats)
summstats


#random forest
require(randomForest)
train$ABOVE25_MORT<- as.factor(train$ABOVE25_MORT)
for (i in 1:ncol(train)){
  train[is.na(train[,i]),i] <- mean(train[,i], na.rm=TRUE)
  mort[is.na(mort[,i]),i] <- mean(mort[,i], na.rm=TRUE)
}
rand_train<- randomForest(train$ABOVE25_MORT ~., data=train, na.action = na.omit, importance=TRUE)
print(rand_train)
predTrain<- predict(rand_train, mort, type="class")
randtab<- table(predTrain, mort$ABOVE25_MORT)

mat<- confmat(randtab)

stats<- as.data.frame(matrix(c("Random Forest(ntree=500)",  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
summstats<- rbind(summstats, stats)
summstats
dev.off()
par(mar= c(4,15,1,1),las=1, cex.axis = .5)

vargini<- varImpPlot(rand_train, sort=FALSE)[,"MeanDecreaseGini"]
dat<- vargini[order(vargini, decreasing=FALSE)]
barplot(dat, horiz=TRUE, main="Random Forest Importance", xlab="Mean Decrease Gini", col=blues9)

#reduce dataframe
vargini1<- as.data.frame(varImpPlot(rand_train, sort=FALSE)[,"MeanDecreaseGini"])
n<- as.data.frame(row.names(vargini1))
vargini1<- cbind(n, vargini1)
new_name<- c("Var", "MeanDecreaseGini")
names(vargini1)<- new_name
row.names(vargini1) = NULL
dat_new<- vargini1[order(vargini1$MeanDecreaseGini, decreasing = T),]

gini_vars<- c(dat_new$Var[1:5])
#Mean Decrease Accuracy
varacc<- varImpPlot(rand_train, sort=FALSE)[,"MeanDecreaseAccuracy"]
dat1<- varacc[order(varacc, decreasing=FALSE)]
barplot(dat1, horiz=TRUE, main="Random Forest Importance", xlab="Mean Decrease Accuracy", col=blues9)

varacc1<- as.data.frame(varImpPlot(rand_train, sort=FALSE)[,"MeanDecreaseAccuracy"])
n<- as.data.frame(row.names(varacc1))
varacc1<- cbind(n, varacc1)
new_name<- c("Var", "MeanDecreaseAccuracy")
names(varacc1)<- new_name
row.names(varacc1) = NULL
dat_new1<- varacc1[order(varacc1$MeanDecreaseAccuracy,decreasing = T),]

acc_vars<- c(dat_new1$Var[1:5])
#need to run models on these new dfs



dev.off()
#knn classification
library(class)

knnstats<- as.data.frame(matrix(c(NA), ncol=5))
name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
names(knnstats)<- name

for (i in 1:10){
  knnmodel<- knn(train[,-which(names(train) %in% drop2)], mort[,-which(names(mort) %in% drop2)], train$ABOVE25_MORT, k=i)
  
  knntab <- table(knnmodel,mort$ABOVE25_MORT)
  
  #print(str(i))
  mat<- confmat(knntab)
  knntype<- paste("KNN (", i, ")", sep="", collapse=NULL)
  
  stats<- as.data.frame(matrix(c(knntype, mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  knnstats<- rbind(knnstats, stats)
}

max_acc_knn<- knnstats[order(knnstats$Accuracy, decreasing=T),]
summstats<- rbind(summstats, max_acc_knn[1,])


######GINI INCREASE DATAFRAME

##gini_vars

#gini df
gini_train<- train[,c(gini_vars, "ABOVE25_MORT")]
gini_mort<- mort[, c(gini_vars, "ABOVE25_MORT")]

ginistats<- as.data.frame(matrix(c(NA), ncol=5))
name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
names(ginistats)<- name
#decision trees (classification) - rpart
mo<- rpart(gini_train$ABOVE25_MORT ~., data=gini_train, method="class")
#mo<- tree(as.factor(ABOVE25_MORT) ~., data=gini_train, split=c("gini"))
mo

#deviance is a bit better than gini - why is that?
dev.off()
#par(mar=c(1,1,1,1))
#rpart.plot(mo)
plot(mo, uniform=T)
text(mo, pretty=0)

dcpredict<- predict(mo, newdata = gini_mort, type="class")
dctab<- table(dcpredict, gini_mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
stats<- as.data.frame(matrix(c("Decision Tree", mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
ginistats<- rbind(ginistats, stats)
ginistats<- ginistats[complete.cases(ginistats),]

#pruning
min_cp<- mo$cptable[which.min(mo$cptable[,"xerror"]), "CP"]
tree_prune<- prune(mo, cp = min_cp)
#cv_prune<-cv.tree(mo, FUN = prune.misclass)
#cv_prune
#tree_prune<- prune.misclass(mo, best=18)
#rpart.plot(tree_prune)
plot(tree_prune, uniform=T)
text(tree_prune, pretty=0)

dcpredict<- predict(tree_prune, newdata = gini_mort, type=c("class"))
dctab<- table(dcpredict, gini_mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
#stats<- as.data.frame(matrix(c("Decision Tree (Pruned, cp=.02)",  mat[1], mat[2], 
#    mat[3], mat[4]), ncol=5))
dc_prune<- paste("Decision Tree (Pruned, CP = ", round(min_cp, digits=5), ")", sep="", collapse=NULL)

stats<- as.data.frame(matrix(c(dc_prune,  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
ginistats<- rbind(ginistats, stats)

#stopping criteria
mo_stop<- rpart(gini_train$ABOVE25_MORT ~., data=gini_train, method="class", control = rpart.control(minsplit=5))
#mo_stop<- tree(as.factor(ABOVE25_MORT) ~., data=gini_train, split=c("gini"), control = tree.control(nrow(train), mincut= 5))
mo_stop
#rpart.plot(mo_stop)
plot(mo_stop)

dcpredict<- predict(mo_stop, newdata = gini_mort, type=c("class"))
dctab<- table(dcpredict, gini_mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
stats<- as.data.frame(matrix(c("Decision Tree (Minsplit = 5)",  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
ginistats<- rbind(ginistats, stats)

ginistats


#random forest
require(randomForest)
gini_train$ABOVE25_MORT<- as.factor(gini_train$ABOVE25_MORT)
for (i in 1:ncol(gini_train)){
  gini_train[is.na(gini_train[,i]),i] <- mean(gini_train[,i], na.rm=TRUE)
  gini_mort[is.na(gini_mort[,i]),i] <- mean(gini_mort[,i], na.rm=TRUE)
}
rand_train<- randomForest(gini_train$ABOVE25_MORT ~., data=gini_train, na.action = na.omit, importance=TRUE)
print(rand_train)
predTrain<- predict(rand_train, gini_mort, type="class")
randtab<- table(predTrain, gini_mort$ABOVE25_MORT)

mat<- confmat(randtab)

stats<- as.data.frame(matrix(c("Random Forest(ntree=500)",  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
ginistats<- rbind(ginistats, stats)
ginistats

#par(mar= c(4,15,1,1),las=1, cex.axis = .5)

#vargini<- varImpPlot(rand_train, sort=FALSE)[,"MeanDecreaseGini"]
#dat<- vargini[order(vargini, decreasing=FALSE)]
#barplot(dat, horiz=TRUE, main="Random Forest Importance", xlab="Mean Decrease Gini", col=blues9)
##other plots


#knn

knnstats<- as.data.frame(matrix(c(NA), ncol=5))
name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
names(knnstats)<- name

for (i in 1:10){
  knnmodel<- knn(gini_train[,-which(names(gini_train) %in% drop2)], gini_mort[,-which(names(gini_mort) %in% drop2)], gini_train$ABOVE25_MORT, k=i)
  
  knntab <- table(knnmodel,gini_mort$ABOVE25_MORT)
  
  #print(str(i))
  mat<- confmat(knntab)
  knntype<- paste("KNN (", i, ")", sep="", collapse=NULL)
  
  stats<- as.data.frame(matrix(c(knntype, mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  knnstats<- rbind(knnstats, stats)
}

max_acc_knn<- knnstats[order(knnstats$Accuracy, decreasing=T),]
ginistats<- rbind(ginistats, max_acc_knn[1,])
ginistats


###MEAN ACCRUACY DECREASE DATAFRAME


##acc_vars
acc_train<- train[,c(acc_vars, "ABOVE25_MORT")]
acc_mort<- mort[, c(acc_vars, "ABOVE25_MORT")]

accstats<- as.data.frame(matrix(c(NA), ncol=5))
name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
names(accstats)<- name

#decision trees (classification) - rpart
mo<- rpart(acc_train$ABOVE25_MORT ~., data=acc_train, method="class")
#mo<- tree(as.factor(ABOVE25_MORT) ~., data=acc_train, split=c("gini"))
mo

#deviance is a bit better than gini - why is that?
dev.off()
#par(mar=c(1,1,1,1))
#rpart.plot(mo)
plot(mo, uniform=T)
text(mo, pretty=0)

dcpredict<- predict(mo, newdata = acc_mort, type="class")
dctab<- table(dcpredict, acc_mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
stats<- as.data.frame(matrix(c("Decision Tree", mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
accstats<- rbind(accstats, stats)
accstats<- accstats[complete.cases(accstats),]

#pruning
min_cp<- mo$cptable[which.min(mo$cptable[,"xerror"]), "CP"]
tree_prune<- prune(mo, cp = min_cp)
#cv_prune<-cv.tree(mo, FUN = prune.misclass)
#cv_prune
#tree_prune<- prune.misclass(mo, best=20)
#rpart.plot(tree_prune)
plot(tree_prune, uniform=T)
text(tree_prune, pretty=0)

dcpredict<- predict(tree_prune, newdata = acc_mort, type=c("class"))
dctab<- table(dcpredict, acc_mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
#stats<- as.data.frame(matrix(c("Decision Tree (Pruned, cp=.02)",  mat[1], mat[2], 
#    mat[3], mat[4]), ncol=5))
dc_prune<- paste("Decision Tree (Pruned, CP = ", round(min_cp, digits=5), ")", sep="", collapse=NULL)

stats<- as.data.frame(matrix(c(dc_prune,  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
accstats<- rbind(accstats, stats)
accstats
#stopping criteria
mo_stop<- rpart(train$ABOVE25_MORT ~., data=acc_train, method="class", control = rpart.control(minsplit=5))
#mo_stop<- tree(as.factor(ABOVE25_MORT) ~., data=acc_train, split=c("gini"), control = tree.control(nrow(train), mincut= 5))
mo_stop
#rpart.plot(mo_stop)
plot(mo_stop)

dcpredict<- predict(mo_stop, newdata = acc_mort, type=c("class"))
dctab<- table(dcpredict, acc_mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
stats<- as.data.frame(matrix(c("Decision Tree (Minsplit = 5)",  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
accstats<- rbind(accstats, stats)

accstats


#random forest
require(randomForest)
train$ABOVE25_MORT<- as.factor(acc_train$ABOVE25_MORT)
for (i in 1:ncol(acc_train)){
  acc_train[is.na(acc_train[,i]),i] <- mean(acc_train[,i], na.rm=TRUE)
  acc_mort[is.na(acc_mort[,i]),i] <- mean(acc_mort[,i], na.rm=TRUE)
}
rand_train<- randomForest(acc_train$ABOVE25_MORT ~., data=acc_train, na.action = na.omit, importance=TRUE)
print(rand_train)
predTrain<- predict(rand_train, acc_mort, type="class")
randtab<- table(predTrain, acc_mort$ABOVE25_MORT)

mat<- confmat(randtab)

stats<- as.data.frame(matrix(c("Random Forest(ntree=500)",  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
accstats<- rbind(accstats, stats)
accstats

#par(mar= c(4,15,1,1),las=1, cex.axis = .5)

#vargini<- varImpPlot(rand_train, sort=FALSE)[,"MeanDecreaseGini"]
#dat<- vargini[order(vargini, decreasing=FALSE)]
#barplot(dat, horiz=TRUE, main="Random Forest Importance", xlab="Mean Decrease Gini", col=blues9)

#knn

knnstats<- as.data.frame(matrix(c(NA), ncol=5))
name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
names(knnstats)<- name

for (i in 1:10){
  knnmodel<- knn(acc_train[,-which(names(acc_train) %in% drop2)], acc_mort[,-which(names(acc_mort) %in% drop2)], acc_train$ABOVE25_MORT, k=i)
  
  knntab <- table(knnmodel,acc_mort$ABOVE25_MORT)
  
  #print(str(i))
  mat<- confmat(knntab)
  knntype<- paste("KNN (", i, ")", sep="", collapse=NULL)
  
  stats<- as.data.frame(matrix(c(knntype, mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  knnstats<- rbind(knnstats, stats)
}

max_acc_knn<- knnstats[order(knnstats$Accuracy, decreasing=T),]
accstats<- rbind(accstats, max_acc_knn[1,])
accstats

for (i in 2:ncol(summstats)){
  summstats[,i]<- as.numeric(summstats[,i])
  ginistats[,i]<- as.numeric(ginistats[,i])
  accstats[,i]<- as.numeric(accstats[,i])
}

diffstats<- as.data.frame(matrix(c(NA), ncol=4, nrow=5))
name<- c("Model", "Base vs. Gini", "Base vs. AccDecrease", "Gini vs. AccDecrease")
names(diffstats)<- name

diffstats[,"Model"]<- summstats$Model
diffstats[2, "Model"]<- c("Decision Tree (Pruned)")
diffstats[5, "Model"]<- c("KNN")
diffstats[,"Base vs. Gini"]<- summstats$Accuracy - ginistats$Accuracy
diffstats[,"Base vs. AccDecrease"]<- summstats$Accuracy - accstats$Accuracy
diffstats[,"Gini vs. AccDecrease"]<- ginistats$Accuracy - accstats$Accuracy
diffstats


library(corrplot)
library(dplyr)
corrplot(cor(train[, 1:(ncol(train)-2)]), order = "hclust")
corr<- as.data.frame(cor(train[, 1:(ncol(train)-2)]))
heatmap(corr,margins = c(10,10))
heatmaply_cor(corr)

corr_high<- as.data.frame(corr[apply((corr>=.9) & (corr!=1),1,any),apply((corr>=.9) & (corr!=1),1,any)])
corr_high
heatmaply_cor(corr_high)

corr_inv<- as.data.frame(corr[apply((corr<=-.9),1,any),apply((corr<= -.9),1,any)])
corr_inv
heatmaply_cor(corr_inv)

reduce_corr<- corr[union(which(names(corr) %in% gini_vars), which(names(corr) %in% acc_vars)),
                   union(which(names(corr) %in% gini_vars), which(names(corr) %in% acc_vars))]
dim(reduce_corr)
heatmaply_cor(reduce_corr)

gini_corr<- cor(gini_train[, 1:(ncol(gini_train)-1)])
heatmaply_cor(gini_corr)

gini_corr_high<- as.data.frame(gini_corr[apply((gini_corr>=.89),1,all),])
gini_corr_high
dim(gini_corr_high)

gini_corr[which(row.names(gini_corr) %in% row.names(gini_corr[apply((gini_corr>=.89),1,all)])),]

acc_corr<- cor(acc_train[, 1:(ncol(acc_train)-1)])
heatmaply_cor(acc_corr)


###ONLY 1 Variable
one_train<- train[,c("ATLEAST_BASIC_DRINK", "ABOVE25_MORT")]
one_mort<- mort[, c("ATLEAST_BASIC_DRINK", "ABOVE25_MORT")]

onestats<- as.data.frame(matrix(c(NA), ncol=5))
name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
names(onestats)<- name

#decision trees (classification) - rpart
mo<- rpart(one_train$ABOVE25_MORT ~., data=one_train, method="class")
#mo<- tree(as.factor(ABOVE25_MORT) ~., data=acc_train, split=c("gini"))
mo

#deviance is a bit better than gini - why is that?
dev.off()
#par(mar=c(1,1,1,1))
#rpart.plot(mo)
plot(mo, uniform=T)
text(mo, pretty=0)

dcpredict<- predict(mo, newdata = one_mort, type="class")
dctab<- table(dcpredict, one_mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
stats<- as.data.frame(matrix(c("Decision Tree", mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
onestats<- rbind(onestats, stats)
onestats<- onestats[complete.cases(onestats),]

#pruning
min_cp<- mo$cptable[which.min(mo$cptable[,"xerror"]), "CP"]
tree_prune<- prune(mo, cp = min_cp)
#cv_prune<-cv.tree(mo, FUN = prune.misclass)
#cv_prune
#tree_prune<- prune.misclass(mo, best=20)
#rpart.plot(tree_prune)
plot(tree_prune, uniform=T)
text(tree_prune, pretty=0)

dcpredict<- predict(tree_prune, newdata = one_mort, type=c("class"))
dctab<- table(dcpredict, one_mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
#stats<- as.data.frame(matrix(c("Decision Tree (Pruned, cp=.02)",  mat[1], mat[2], 
#    mat[3], mat[4]), ncol=5))
dc_prune<- paste("Decision Tree (Pruned, CP = ", round(min_cp, digits=5), ")", sep="", collapse=NULL)

stats<- as.data.frame(matrix(c(dc_prune,  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
onestats<- rbind(onestats, stats)
onestats
#stopping criteria
mo_stop<- rpart(one_train$ABOVE25_MORT ~., data=one_train, method="class", control = rpart.control(minsplit=5))
#mo_stop<- tree(as.factor(ABOVE25_MORT) ~., data=acc_train, split=c("gini"), control = tree.control(nrow(train), mincut= 5))
mo_stop
#rpart.plot(mo_stop)
plot(mo_stop)

dcpredict<- predict(mo_stop, newdata = one_mort, type=c("class"))
dctab<- table(dcpredict, one_mort[, "ABOVE25_MORT"])

mat<- confmat(dctab)
stats<- as.data.frame(matrix(c("Decision Tree (Minsplit = 5)",  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
onestats<- rbind(onestats, stats)

onestats


#random forest
require(randomForest)
train$ABOVE25_MORT<- as.factor(acc_train$ABOVE25_MORT)
for (i in 1:ncol(one_train)){
  one_train[is.na(one_train[,i]),i] <- mean(one_train[,i], na.rm=TRUE)
  one_mort[is.na(one_mort[,i]),i] <- mean(one_mort[,i], na.rm=TRUE)
}
rand_train<- randomForest(one_train$ABOVE25_MORT ~., data=one_train, na.action = na.omit, importance=TRUE)
print(rand_train)
predTrain<- predict(rand_train, one_mort, type="class")
randtab<- table(predTrain, one_mort$ABOVE25_MORT)

mat<- confmat(randtab)

stats<- as.data.frame(matrix(c("Random Forest(ntree=500)",  mat[1], mat[2], 
                               mat[3], mat[4]), ncol=5))

names(stats)<- name
onestats<- rbind(onestats, stats)
onestats

#par(mar= c(4,15,1,1),las=1, cex.axis = .5)

#vargini<- varImpPlot(rand_train, sort=FALSE)[,"MeanDecreaseGini"]
#dat<- vargini[order(vargini, decreasing=FALSE)]
#barplot(dat, horiz=TRUE, main="Random Forest Importance", xlab="Mean Decrease Gini", col=blues9)

#knn

knnstats<- as.data.frame(matrix(c(NA), ncol=5))
name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
names(knnstats)<- name

for (i in 1:10){
  knnmodel<- knn(as.data.frame(one_train[,-which(names(one_train) %in% drop2)]), 
                 as.data.frame(one_mort[,-which(names(one_mort) %in% drop2)]), one_train$ABOVE25_MORT, k=i)
  
  knntab <- table(knnmodel,one_mort$ABOVE25_MORT)
  
  #print(str(i))
  mat<- confmat(knntab)
  knntype<- paste("KNN (", i, ")", sep="", collapse=NULL)
  
  stats<- as.data.frame(matrix(c(knntype, mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  knnstats<- rbind(knnstats, stats)
}

max_acc_knn<- knnstats[order(knnstats$Accuracy, decreasing=T),]
onestats<- rbind(onestats, max_acc_knn[1,])
onestats

for (i in 2:ncol(summstats)){
  summstats[,i]<- as.numeric(summstats[,i])
  ginistats[,i]<- as.numeric(ginistats[,i])
  accstats[,i]<- as.numeric(accstats[,i])
  onestats[,i]<- as.numeric(onestats[,i])
}

diffstats[,"Base vs. One Var"]<- summstats$Accuracy - onestats$Accuracy
diffstats[,"Gini vs. One Var"]<- ginistats$Accuracy - onestats$Accuracy
diffstats[,"AccDecrease vs. One Var"]<- accstats$Accuracy - onestats$Accuracy
diffstats


#########OTHER MODELS############




#decision trees (regression)
train_u5<- cbind(train, u5_train)
mort_u5<- cbind(mort, u5_mort)

drop2<- c("ABOVE25_MORT")
train_u5<- train_u5[,-which(names(train_u5) %in% drop2)]
mort_u5<- mort_u5[,-which(names(mort_u5) %in% drop2)]

moreg<- rpart(train_u5$UNDER5_MORT ~., data=train_u5, method="anova")
moreg
rpart.plot(moreg)

dcpredictreg<- predict(moreg, newdata = mort_u5)
mse_reg<- sum( (dcpredictreg - mort_u5$UNDER5_MORT) ^ 2)
mse_reg

#pruning
tree_prune<- prune(moreg, cp = .02)
tree_prune
rpart.plot(tree_prune)

dcpredict_prune<- predict(tree_prune, newdata = mort_u5)
mse_reg<- sum( (dcpredict_prune - mort_u5$UNDER5_MORT) ^ 2)
mse_reg




