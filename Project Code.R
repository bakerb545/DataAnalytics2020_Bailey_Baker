rm(list=ls())

library(readxl)
library(rpart)
library(rpart.plot)

mort<- data.frame(read_xlsx("C:/Users/phoen/Downloads/2017 Under-Five Mortality.xlsx"))
#View(mort)

for (i in 3:ncol(mort)-1){
  mort[,i]<- as.numeric(mort[,i])
}
summary(mort)
track<- data.frame("Name" = NA, "NAs" = NA)
mort[, "POVERTY"]<- sub("-", NA, mort[,"POVERTY"])

for (i in 1:ncol(mort)){
  track1<- data.frame("Name" = names(mort)[i], 
                      "NAs" =sum(!complete.cases(mort[,i])))
  track<- rbind(track, track1)

}
track
mort<- mort[is.na(mort$UNDER5_MORT) == FALSE,]

mort_rate<- mort[,'UNDER5_MORT']
hist(mort_rate)
unimprov_water<- mort[,'UNIMPROVED_DRINK']
print(mean(mort_rate))
mort[, 'ABOVE25_MORT']<- 0
mort[which(mort$UNDER5_MORT>= 25), 'ABOVE25_MORT'] <- 1

boxplot(unimprov_water)
qqnorm(unimprov_water)
summary(mort_rate)
lis<- list("Indicator", "INF_MORT", "CHILD_MORT14", "UNDER5_DEATHS", "CHILD_DEATHS14", "POVERTY", "NO_WASTE_OVERWEIGHT_STUNT", "WASTE_OVERWEIGHT_STUNT")
mort<- mort[,-which(names(mort) %in% lis)]
high_mort<- mort[(mort$UNDER5_MORT >= mean(mort_rate, na.rm=TRUE)),]
low_mort<- mort[(mort$UNDER5_MORT < mean(mort_rate, na.rm=TRUE)),]
summary(high_mort$UNDER5_MORT)
hist(high_mort$UNDER5_MORT)
summary(low_mort$UNDER5_MORT)
hist(low_mort$UNDER5_MORT)

unimprov_water_low<- low_mort$UNIMPROVED_DRINK
unimprov_water_high<- high_mort$UNIMPROVED_DRINK

summary(unimprov_water_high)
summary(unimprov_water_low)

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
train[, 'ABOVE25_MORT']<- 0
train[which(train$UNDER5_MORT>= 25), 'ABOVE25_MORT'] <- 1

summary(train)

summstats<- as.data.frame(matrix(c(NA), ncol=5))
name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
names(summstats)<- name

drop<- c("UNDER5_MORT")
u5_train<- as.data.frame(train[,"UNDER5_MORT"])
u5_mort<- as.data.frame(mort[,"UNDER5_MORT"])
names(u5_train)<- c("UNDER5_MORT")
names(u5_mort)<- c("UNDER5_MORT")
train<- train[,-which(names(train) %in% drop)]
mort<- mort[,-which(names(mort) %in% drop)]

#decision trees (classification) - rpart
mo<- rpart(train$ABOVE25_MORT ~., data=train, method="class",control = list(minsplit=20))
mo
rpart.plot(mo)

dcpredict<- predict(mo, newdata = mort, type=c("class"))
dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])
#confusion matrix
tp = dctab[4]
fn = dctab[3]
fp = dctab[2]
tn = dctab[1]
total_recs<- sum(tp, tn, fn, fp)
recall_rate<- round(tp/(tp+fn),5)
precision_rate<- round(tp/(tp+fp),5)
accuracy_rate<- round((tp+tn)/total_recs,5)
misclassification_rate<- round((fp+fn)/total_recs,5)

stats<- as.data.frame(matrix(c("Decision Tree", recall_rate, precision_rate, 
                               accuracy_rate, misclassification_rate), ncol=5))

names(stats)<- name
summstats<- rbind(summstats, stats)
summstats<- summstats[complete.cases(summstats),]

#pruning
tree_prune<- prune(mo, cp = .02)
tree_prune
rpart.plot(tree_prune)

dcpredict<- predict(tree_prune, newdata = mort, type=c("class"))
dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])

tp = dctab[4]
fn = dctab[3]
fp = dctab[2]
tn = dctab[1]
total_recs<- sum(tp, tn, fn, fp)
recall_rate<- round(tp/(tp+fn),5)
precision_rate<- round(tp/(tp+fp),5)
accuracy_rate<- round((tp+tn)/total_recs,5)
misclassification_rate<- round((fp+fn)/total_recs,5)

stats<- as.data.frame(matrix(c("Decision Tree (Pruned, cp=.02)", recall_rate, precision_rate, 
                               accuracy_rate, misclassification_rate), ncol=5))

names(stats)<- name
summstats<- rbind(summstats, stats)

#stopping criteria
mo_stop<- rpart(train$ABOVE25_MORT ~., data=train, method="class", control = rpart.control(minsplit=5))
mo_stop
rpart.plot(mo_stop)

dcpredict<- predict(mo_stop, newdata = mort, type=c("class"))
dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])

tp = dctab[4]
fn = dctab[3]
fp = dctab[2]
tn = dctab[1]
total_recs<- sum(tp, tn, fn, fp)
recall_rate<- round(tp/(tp+fn),5)
precision_rate<- round(tp/(tp+fp),5)
accuracy_rate<- round((tp+tn)/total_recs,5)
misclassification_rate<- round((fp+fn)/total_recs,5)

stats<- as.data.frame(matrix(c("Decision Tree (Minsplit = 5)", recall_rate, precision_rate, 
                               accuracy_rate, misclassification_rate), ncol=5))

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

tp = randtab[4]
fn = randtab[3]
fp = randtab[2]
tn = randtab[1]
total_recs<- sum(tp, tn, fn, fp)
recall_rate<- round(tp/(tp+fn),5)
precision_rate<- round(tp/(tp+fp),5)
accuracy_rate<- round((tp+tn)/total_recs,5)
misclassification_rate<- round((fp+fn)/total_recs,5)

stats<- as.data.frame(matrix(c("Random Forest(ntree=500)", recall_rate, precision_rate, 
                               accuracy_rate, misclassification_rate), ncol=5))

names(stats)<- name
summstats<- rbind(summstats, stats)
summstats

par(mar= c(4,15,1,1),las=1, cex.axis = .5)
vargini<- varImpPlot(rand_train, sort=FALSE)[,"MeanDecreaseGini"]
dat<- vargini[order(vargini, decreasing=FALSE)]
barplot(dat, horiz=TRUE, main="Random Forest Importance", xlab="Mean Decrease Gini", col=blues9)

varacc<- varImpPlot(rand_train, sort=FALSE)[,"MeanDecreaseAccuracy"]
dat1<- varacc[order(varacc, decreasing=FALSE)]
barplot(dat1, horiz=TRUE, main="Random Forest Importance", xlab="Mean Decrease Accuracy", col=blues9)

par()
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

#knn classification
library(class)

for (i in 1:10){
  knnmodel<- knn(train[,-which(names(train) %in% drop2)], mort[,-which(names(mort) %in% drop2)], train$ABOVE25_MORT, k=i)
  attributes(knnmodel)
  
  knntab <- table(knnmodel,mort$ABOVE25_MORT)
  knntab
  
  tp = knntab[4]
  fn = knntab[3]
  fp = knntab[2]
  tn = knntab[1]
  total_recs<- sum(tp, tn, fn, fp)
  recall_rate<- round(tp/(tp+fn),5)
  precision_rate<- round(tp/(tp+fp),5)
  accuracy_rate<- round((tp+tn)/total_recs,5)
  misclassification_rate<- round((fp+fn)/total_recs,5)
  #print(str(i))
  knntype<- paste("KNN (", i, ")", sep="", collapse=NULL)
  
  stats<- as.data.frame(matrix(c(knntype, recall_rate, precision_rate, 
                                 accuracy_rate, misclassification_rate), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)
}
summstats

#clustering
library(stats)

predkmeans<- kmeans(train[,-which(names(train) %in% drop2)], centers=2, iter.max = 1000)
print(table(train$ABOVE25_MORT, predkmeans$cluster))
train[,'Cluster']<- factor(predkmeans$cluster)

#
predkmeans_mort<- kmeans(mort[,-which(names(mort) %in% drop2)], centers=2, iter.max = 1000)
print(table(mort$ABOVE25_MORT, predkmeans_mort$cluster))
mort[,'Cluster']<- factor(predkmeans_mort$cluster)


clust_mult_centers<- as.data.frame(predkmeans$centers)
cent_mort<- as.data.frame(predkmeans_mort$centers)
library(ggplot2)

ggplot() +
  geom_point(data = train, 
             mapping = aes(x =AT_LEAST_BASIC_SANIT, 
                           y = IMPROVED_DRINK_PREMISES, 
                           colour = Cluster))+
  geom_point(data = clust_mult_centers, aes(x = AT_LEAST_BASIC_SANIT, 
                           y = IMPROVED_DRINK_PREMISES,
                  color = 'Center'),size = 5)


#mort
ggplot() +
  geom_point(data = mort, 
             mapping = aes(x =AT_LEAST_BASIC_SANIT, 
                           y = IMPROVED_DRINK_PREMISES, 
                           colour = Cluster))+
  geom_point(data = cent_mort, aes(x = AT_LEAST_BASIC_SANIT, 
                                            y = IMPROVED_DRINK_PREMISES,
                                            color = 'Center'),size = 5)

  
#maybe use distance metrics to see how far the cluster centers for the 
#attributes are apart

clust_dist<- data.frame("Name" = NA, "Dist" = NA)
for (i in 1:dim(clust_mult_centers)[2]){
  dist1<- data.frame("Name" = names(clust_mult_centers)[i], 
                      "Dist" = (clust_mult_centers[1, i] - clust_mult_centers[2, i])^2)
  #print(track1)
  clust_dist<- rbind(clust_dist, dist1)
}

clust_dist<- clust_dist[complete.cases(clust_dist),]
clust_dist <- clust_dist[order(clust_dist$Dist, decreasing = TRUE), ]
clust_dist


ggplot() +
  geom_point(data = train, 
             mapping = aes(x =AT_LEAST_BASIC_SANIT, 
                           y = IMPROVED_DRINK_PREMISES, 
                           colour = ABOVE25_MORT))


clust_dist_mort<- data.frame("Name" = NA, "Dist" = NA)
for (i in 1:dim(cent_mort)[2]){
  dist1<- data.frame("Name" = names(cent_mort)[i], 
                     "Dist" = (cent_mort[1, i] - cent_mort[2, i])^2)
  #print(track1)
  clust_dist_mort<- rbind(clust_dist_mort, dist1)
}

clust_dist_mort<- clust_dist_mort[complete.cases(clust_dist_mort),]
clust_dist_mort <- clust_dist_mort[order(clust_dist_mort$Dist, decreasing = TRUE), ]
clust_dist_mort


ggplot() +
  geom_point(data =mort, 
             mapping = aes(x =AT_LEAST_BASIC_SANIT, 
                           y = IMPROVED_DRINK_PREMISES, 
                           colour = ABOVE25_MORT))

train_u5[,'Cluster']<- factor(predkmeans$cluster)
mort_u5[,'Cluster']<- factor(predkmeans_mort$cluster)


train_clust1 <- train_u5[train_u5$Cluster == 1,]
train_clust2 <- train_u5[train_u5$Cluster == 2,]

mort_clust1 <- mort_u5[mort_u5$Cluster == 1,]
mort_clust2 <- mort_u5[mort_u5$Cluster == 2,]

hist(train_clust1$UNDER5_MORT)
hist(train_clust2$UNDER5_MORT)
hist(mort_clust1$UNDER5_MORT)
hist(mort_clust2$UNDER5_MORT)

hist(train_clust1$AT_LEAST_BASIC_SANIT)
summary(train_clust1$AT_LEAST_BASIC_SANIT)
hist(train_clust2$AT_LEAST_BASIC_SANIT)
summary(train_clust2$AT_LEAST_BASIC_SANIT)
hist(mort_clust1$IMPROVED_DRINK_PREMISES)
summary(mort_clust1$IMPROVED_DRINK_PREMISES)
hist(mort_clust2$IMPROVED_DRINK_PREMISES)
summary(mort_clust2$IMPROVED_DRINK_PREMISES)

#


#other models
#mo1<- rpart(low_mort$UNDER5_MORT ~., data=low_mort)
#mo1
#rpart.plot(mo1)

#mo2<- rpart(high_mort$UNDER5_MORT ~., data=high_mort)
#mo2
#rpart.plot(mo2)
