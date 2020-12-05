rm(list=ls())

set.seed(1)
library(readxl)
library(rpart)
library(rpart.plot)
library(zoo)
library(tree)
library(class)
library(stats)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(plotly)
library(dplyr)
library(heatmaply)
library(caret)
library(purrr)
library(tidyr)

######FUNCTIONS - CONFUSION MATRIX AND MODELS######

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
  
  dcpredict<- predict(mo, newdata = mort, type="class")
  dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])
  
  mat<- confmat(dctab)
  stats<- as.data.frame(matrix(c("Decision Tree", mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)
  summstats<- summstats[complete.cases(summstats),]

  #pruning
  min_cp<- mo$cptable[which.min(mo$cptable[,"xerror"]), "CP"]
  tree_prune<- prune(mo, cp =min_cp)
  
  dcpredict<- predict(tree_prune, newdata = mort, type=c("class"))
  dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])
  
  mat<- confmat(dctab)
  dc_prune<- paste("Decision Tree (Pruned, CP = ", round(min_cp, digits=5), ")", sep="", collapse=NULL)
  
  stats<- as.data.frame(matrix(c(dc_prune,  mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)

  #stopping criteria
  mo_stop<- rpart(train$ABOVE25_MORT ~., data=train, method="class", control = rpart.control(minsplit=5))
  #mo_stop<- tree(as.factor(ABOVE25_MORT) ~., data=train, split=c("gini"), control = tree.control(nrow(train), mincut= 5))
  #rpart.plot(mo_stop)
  dcpredict<- predict(mo_stop, newdata = mort, type=c("class"))
  dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])
  
  mat<- confmat(dctab)
  stats<- as.data.frame(matrix(c("Decision Tree (Minsplit = 5)",  mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)

  #random forest
  require(randomForest)
  train$ABOVE25_MORT<- as.factor(train$ABOVE25_MORT)
  
  rand_train<- randomForest(train$ABOVE25_MORT ~., data=train, na.action = na.omit, importance=TRUE)
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
    knnmodel<- knn(as.data.frame(train[,-which(names(train) %in% drop2)]),
                   as.data.frame(mort[,-which(names(mort) %in% drop2)]), train$ABOVE25_MORT, k=i)
    
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
  max_acc_knn<- max_acc_knn[complete.cases(max_acc_knn),]
  
  summstats<- rbind(summstats, max_acc_knn[1,])
  
  lis<- list("Statistics" = summstats, "DecTree"= mo, "Pruned DecTree"= tree_prune, "Stopped DecTree"= mo_stop,
             "RF"= rand_train, "KNN stats"= max_acc_knn)
  return(lis)
  
}


##########EXTRAS?##############
#maybe do cp=0 for an unrestricted tree but more cross validations to account for that

#####DATA UPLOAD#####


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
track["Percent"]<- track[, "NAs"]/nrow(mort)
track<- track[order(track$Percent, decreasing=TRUE),]
track

mort_rate<- mort[,'UNDER5_MORT']
hist(mort_rate)
print(mean(mort_rate))
mort[, 'ABOVE25_MORT']<- 0
mort[which(mort$UNDER5_MORT>= 25), 'ABOVE25_MORT'] <- 1

summary(mort_rate)
lis<- list("Indicator", "INF_MORT", "CHILD_MORT14", "UNDER5_DEATHS", "CHILD_DEATHS14", "POVERTY", "NO_WASTE_OVERWEIGHT_STUNT", "WASTE_OVERWEIGHT_STUNT")
mort<- mort[,-which(names(mort) %in% lis)]
print(mean(mort$IMPROVED_DRINK_PREMISES, na.rm=T))

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

for (i in 1:ncol(train)){
  train[is.na(train[,i]),i] <- mean(train[,i], na.rm=TRUE)
  mort[is.na(mort[,i]),i] <- mean(mort[,i], na.rm=TRUE)
}

mort %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#####BASE MODEL #######

base_list= models(train, mort)
basestats <- base_list$Statistics

write.csv(basestats, "c:/Users/phoen/Dropbox/MGMT 6962/Project Results/basestats.csv")

base_tree<- base_list$DecTree
rpart.plot(base_tree)

pruned<- base_list$`Pruned DecTree`
rpart.plot(pruned)

stopped<- base_list$`Stopped DecTree`
rpart.plot(stopped)

basedecvars<- names(base_list$DecTree$variable.importance[1:5])
basedec_train<- train[,c(basedecvars, "ABOVE25_MORT")]
basedec_mort<- mort[,c(basedecvars, "ABOVE25_MORT")]
basedecvars_list= models(basedec_train, basedec_mort)
basedecvarsstats = basedecvars_list$Statistics

####GINI AND ACCURACY DATAFRAME REDUCTION#####
dev.off()
par(mar= c(4,15,1,1),las=1, cex.axis = .5)

rand_train<- base_list$RF
vargini<- importance(rand_train, sort=FALSE)[,"MeanDecreaseGini"]
dat<- vargini[order(vargini, decreasing=FALSE)]
barplot(dat, horiz=TRUE, main="Random Forest Importance", xlab="Mean Decrease Gini", col=blues9)

#reduce dataframe
vargini1<- as.data.frame(importance(rand_train, sort=FALSE)[,"MeanDecreaseGini"])
n<- as.data.frame(row.names(vargini1))
vargini1<- cbind(n, vargini1)
new_name<- c("Var", "MeanDecreaseGini")
names(vargini1)<- new_name
row.names(vargini1) = NULL
dat_new<- vargini1[order(vargini1$MeanDecreaseGini, decreasing = T),]

gini_vars<- c(dat_new$Var[1:5])
#Mean Decrease Accuracy
varacc<- importance(rand_train, sort=FALSE)[,"MeanDecreaseAccuracy"]
dat1<- varacc[order(varacc, decreasing=FALSE)]
barplot(dat1, horiz=TRUE, main="Random Forest Importance", xlab="Mean Decrease Accuracy", col=blues9)

varacc1<- as.data.frame(importance(rand_train, sort=FALSE)[,"MeanDecreaseAccuracy"])
n<- as.data.frame(row.names(varacc1))
varacc1<- cbind(n, varacc1)
new_name<- c("Var", "MeanDecreaseAccuracy")
names(varacc1)<- new_name
row.names(varacc1) = NULL
dat_new1<- varacc1[order(varacc1$MeanDecreaseAccuracy,decreasing = T),]

acc_vars<- c(dat_new1$Var[1:5])
#need to run models on these new dfs

dev.off()

######CLUSTERING######


train_u5<- cbind(train, u5_train)
mort_u5<- cbind(mort, u5_mort)

predkmeans<- kmeans(train[,-which(names(train) %in% drop2)], centers=2, iter.max = 1000)
print(table(train$ABOVE25_MORT, predkmeans$cluster))
train[,'Cluster']<- factor(predkmeans$cluster)

#
predkmeans_mort<- kmeans(mort[,-which(names(mort) %in% drop2)], centers=2, iter.max = 1000)
print(table(mort$ABOVE25_MORT, predkmeans_mort$cluster))
mort[,'Cluster']<- factor(predkmeans_mort$cluster)


clust_mult_centers<- as.data.frame(predkmeans$centers)
cent_mort<- as.data.frame(predkmeans_mort$centers)


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
summary(train_clust1$UNDER5_MORT)
hist(train_clust2$UNDER5_MORT)
summary(train_clust2$UNDER5_MORT)
hist(mort_clust1$UNDER5_MORT)
summary(mort_clust1$UNDER5_MORT)
hist(mort_clust2$UNDER5_MORT)
summary(mort_clust2$UNDER5_MORT)

hist(train_clust1$AT_LEAST_BASIC_SANIT)
summary(train_clust1$AT_LEAST_BASIC_SANIT)
hist(train_clust2$AT_LEAST_BASIC_SANIT)
summary(train_clust2$AT_LEAST_BASIC_SANIT)

hist(train_clust1$IMPROVED_DRINK_PREMISES)
summary(train_clust1$IMPROVED_DRINK_PREMISES)
hist(train_clust2$IMPROVED_DRINK_PREMISES)
summary(train_clust2$IMPROVED_DRINK_PREMISES)

hist(mort_clust1$AT_LEAST_BASIC_SANIT)
summary(mort_clust1$AT_LEAST_BASIC_SANIT)
hist(mort_clust2$AT_LEAST_BASIC_SANIT)
summary(mort_clust2$AT_LEAST_BASIC_SANIT)

hist(mort_clust1$IMPROVED_DRINK_PREMISES)
summary(mort_clust1$IMPROVED_DRINK_PREMISES)
hist(mort_clust2$IMPROVED_DRINK_PREMISES)
summary(mort_clust2$IMPROVED_DRINK_PREMISES)



######GINI INCREASE DATAFRAME#####

##gini_vars

#gini df
gini_train<- train[,c(gini_vars, "ABOVE25_MORT")]
gini_mort<- mort[, c(gini_vars, "ABOVE25_MORT")]

gini_list= models(gini_train, gini_mort)
ginistats <- gini_list$Statistics


base_tree<- gini_list$DecTree
rpart.plot(base_tree)

pruned<- gini_list$`Pruned DecTree`
rpart.plot(pruned)

stopped<- gini_list$`Stopped DecTree`
rpart.plot(stopped)

###MEAN ACCRUACY DECREASE DATAFRAME######


##acc_vars
acc_train<- train[,c(acc_vars, "ABOVE25_MORT")]
acc_mort<- mort[, c(acc_vars, "ABOVE25_MORT")]

acc_list= models(acc_train, acc_mort)
accstats <- acc_list$Statistics

base_tree<- acc_list$DecTree
rpart.plot(base_tree)

pruned<- acc_list$`Pruned DecTree`
rpart.plot(pruned)

stopped<- acc_list$`Stopped DecTree`
rpart.plot(stopped)

####DIFFSTATS####

for (i in 2:ncol(basestats)){
  basestats[,i]<- as.numeric(basestats[,i])
  ginistats[,i]<- as.numeric(ginistats[,i])
  accstats[,i]<- as.numeric(accstats[,i])
}

diffstats<- as.data.frame(matrix(c(NA), ncol=4, nrow=5))
name<- c("Model", "Base vs. Gini", "Base vs. AccDecrease", "Gini vs. AccDecrease")
names(diffstats)<- name

diffstats[,"Model"]<- basestats$Model
diffstats[2, "Model"]<- c("Decision Tree (Pruned)")
diffstats[5, "Model"]<- c("KNN")
diffstats[,"Base vs. Gini"]<- basestats$Accuracy - ginistats$Accuracy
diffstats[,"Base vs. AccDecrease"]<- basestats$Accuracy - accstats$Accuracy
diffstats[,"Gini vs. AccDecrease"]<- ginistats$Accuracy - accstats$Accuracy
diffstats

###CORRELATION PLOTS####


#dev.off()
corrplot(cor(train[, 1:(ncol(train)-2)]), order = "hclust")
corr<- as.data.frame(cor(train[, 1:(ncol(train)-2)]))
#heatmap(as.matrix(corr),margins = c(10,10))
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

acc_corr<- cor(acc_train[, 1:(ncol(acc_train)-1)])
heatmaply_cor(acc_corr)

######ONE VARIABLE EACH FROM GINI AND ACC DFS######

###ONLY 1 Variable of gini df
onevarstats<- as.data.frame(matrix(c(NA), nrow=7, ncol=6))
name<- c("Model", row.names(gini_corr)[1], row.names(gini_corr)[2],
         row.names(gini_corr)[3], row.names(gini_corr)[4],
         row.names(gini_corr)[5])
names(onevarstats)<- name

onevarstats$Model <- c("Decision Tree", "Decision Tree (Pruned)",
                         "Decision Tree (Minsplit = 5)", "Random Forest (ntree=500)",
                         "KNN", "KNN #", "Prune #")
model_list_gini = list()

for (i in row.names(gini_corr)){
  one_train<- train[,c(i, "ABOVE25_MORT")]
  one_mort<- mort[, c(i, "ABOVE25_MORT")]
  one_list <- models(one_train, one_mort)
  model_list_gini[[i]] <- one_list
  one_stats <- one_list$Statistics
  onevarstats[,i]<- c(one_stats$Accuracy, (one_list$`KNN stats`$Model)[1], one_list$`Pruned DecTree`$control$cp)
}

onevarstats
#### one var of acc df
onevaraccstats<- as.data.frame(matrix(c(NA), nrow=7, ncol=6))
name<- c("Model", row.names(acc_corr)[1], row.names(acc_corr)[2],
         row.names(acc_corr)[3], row.names(acc_corr)[4],
         row.names(acc_corr)[5])
names(onevaraccstats)<- name

onevaraccstats$Model <- c("Decision Tree", "Decision Tree (Pruned)",
                       "Decision Tree (Minsplit = 5)", "Random Forest (ntree=500)",
                       "KNN", "KNN #", "Prune #")
model_list_acc = list()

for (i in row.names(acc_corr)){
  one_train<- train[,c(i, "ABOVE25_MORT")]
  one_mort<- mort[, c(i, "ABOVE25_MORT")]
  one_list <- models(one_train, one_mort)
  model_list_acc[[i]] <- one_list
  one_stats <- one_list$Statistics
  onevaraccstats[,i]<- c(one_stats$Accuracy, (one_list$`KNN stats`$Model)[1], one_list$`Pruned DecTree`$control$cp)
}

onevaraccstats

sim_vars <- intersect(gini_vars, acc_vars)

sim_train<- train[,c(sim_vars, "ABOVE25_MORT")]
sim_mort<- mort[, c(sim_vars, "ABOVE25_MORT")]
sim_list<- models(sim_train, sim_mort)
sim_list$Statistics


#####STAT DIFFS - DO NOT USE!!!!!#####

for (i in 2:ncol(basestats)){
  basestats[,i]<- as.numeric(basestats[,i])
  ginistats[,i]<- as.numeric(ginistats[,i])
  accstats[,i]<- as.numeric(accstats[,i])
  onestats[,i]<- as.numeric(onestats[,i])
}

diffstats[,"Base vs. One Var"]<- basestats$Accuracy - onestats$Accuracy
diffstats[,"Gini vs. One Var"]<- ginistats$Accuracy - onestats$Accuracy
diffstats[,"AccDecrease vs. One Var"]<- accstats$Accuracy - onestats$Accuracy
diffstats


#########OTHER MODELS - DO NOT USE!!!!!############




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

