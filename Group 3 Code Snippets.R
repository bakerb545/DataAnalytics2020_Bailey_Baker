rm(list=ls())
#group 3


#lab1 rpart1
require(rpart)
swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart, uniform=T) 
text(swiss_rpart) 
#lab1 rpart2
# Regression Tree Example
require(rpart)
# build the  tree
fitM <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
printcp(fitM) # display the results
plotcp(fitM)
summary(fitM)
par(mfrow=c(1,2)) 
rsq.rpart(fitM) # visualize cross-validation results
# plot tree
plot(fitM, uniform=TRUE, main="Regression Tree for Mileage ")
text(fitM, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
pfitM<- prune(fitM, cp=0.01160389) # from cptable??? adjust this to see the effect
# plot the pruned tree
plot(pfitM, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(pfitM, use.n=TRUE, all=TRUE, cex=.8)
post(pfitM, file ="ptree2.ps", title = "Pruned Regression Tree for Mileage")

#lab1 rpart3
library(e1071)
library(rpart)
data(Glass, package="mlbench")
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]
rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")
printcp(rpart.model)
plotcp(rpart.model)

rsq.rpart(rpart.model)
print(rpart.model)

plot(rpart.model,compress=TRUE)
text(rpart.model, use.n=TRUE)
plot(rpart.pred)

#lab1 rpart4
fitK <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)
printcp(fitK) # display the results
plotcp(fitK) # visualize cross-validation results
summary(fitK) # detailed summary of splits
# plot tree
plot(fitK, uniform=TRUE, main="Classification Tree for Kyphosis")
text(fitK, use.n=TRUE, all=TRUE, cex=.8)
# create attractive postscript plot of tree
post(fitK, file = "kyphosistree.ps", title = "Classification Tree for Kyphosis") # might need to convert to PDF (distill)

pfitK<- prune(fitK, cp=fitK$cptable[which.min(fitK$cptable[,"xerror"]),"CP"])
plot(pfitK, uniform=TRUE, main="Pruned Classification Tree for Kyphosis")
text(pfitK, use.n=TRUE, all=TRUE, cex=.8)
post(pfitK, file = "ptree.ps", title = "Pruned Classification Tree for Kyphosis")

#rpart for Rings on Abalone
abalone<- read.csv("C:/Users/phoen/Downloads/abalone.csv")
train<- sample(nrow(abalone), nrow(abalone)/2)
trainset<- abalone[train,]
testset<- abalone[-train,]
abalone$Rings<- as.factor(abalone$Rings)
fitaba<- rpart(Rings~., data=trainset, method="class")
printcp(fitaba)
plot(fitaba, uniform=T)
text(fitaba, cex=.5)

fittest<- predict(fitaba, newdata=testset,type="class")
head(fittest)
mean(fittest== testset$Rings)
#lab1 ctree1

#lab1 ctree2

#lab1 ctree3

#lab 1 svm labs 1-13 + svm_rpart1


#pg. 9 of kernlab, svmpath and klaR in v15i09


####GROUP 4 LOCAL REGRESSION LDA