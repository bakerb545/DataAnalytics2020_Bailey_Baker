rm(list=ls())

###Lab1
#bronx1
library(gdata)
library(readxl)
bronx1<-read_xls("C:/Users/phoen/Downloads/rollingsales_bronx.xls", sheet=1,skip=4) 

View(bronx1)
attach(bronx1)
`SALE\nPRICE`<- sub("\\$", "", `SALE\nPRICE`)
`SALE\nPRICE`<- as.numeric(gsub(",", "", `SALE\nPRICE`))
`GROSS SQUARE FEET`<-as.numeric(gsub(",","", `GROSS SQUARE FEET`)) 
`LAND SQUARE FEET`<-as.numeric(gsub(",","", `LAND SQUARE FEET`))
bronx1<-bronx1[which(bronx1$`GROSS SQUARE FEET`!=0 & bronx1$`LAND SQUARE FEET`!=0 & bronx1$`SALE\nPRICE`!=0),]
attach(bronx1)
plot(log(`GROSS SQUARE FEET`), log(`SALE\nPRICE`)) 
m1<-lm(log(`SALE\nPRICE`)~log(`GROSS SQUARE FEET`), data=bronx1)
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

m2<-lm(log(bronx1$`SALE\nPRICE`)~log(bronx1$`GROSS SQUARE FEET`)+log(bronx1$`LAND SQUARE FEET`)+factor(bronx1$NEIGHBORHOOD), data=bronx1)
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1$`SALE\nPRICE`)~0+log(bronx1$`GROSS SQUARE FEET`)+log(bronx1$`LAND SQUARE FEET`)+factor(bronx1$NEIGHBORHOOD))
plot(resid(m2a))

m3<-lm(log(bronx1$`SALE\nPRICE`)~0+log(bronx1$`GROSS SQUARE FEET`)+log(bronx1$`LAND SQUARE FEET`)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$`BUILDING CLASS CATEGORY`))
summary(m3)
plot(resid(m3))

m4<-lm(log(bronx1$`SALE\nPRICE`)~log(bronx1$`GROSS SQUARE FEET`)+log(bronx1$`LAND SQUARE FEET`)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$`BUILDING CLASS CATEGORY`))
summary(m4)
plot(resid(m4))

#bronx 2
bronx1$SALE.PRICE<-sub("\\$","",bronx1$`SALE\nPRICE`) 
bronx1$SALE.PRICE<-as.numeric(gsub(",","", bronx1$SALE.PRICE)) 
bronx1$GROSS.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$`GROSS SQUARE FEET`)) 
bronx1$LAND.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$`LAND SQUARE FEET`)) 
bronx1$SALE.DATE<- as.Date(gsub("[^]:digit:]]","",bronx1$`SALE DATE`)) 
bronx1$YEAR.BUILT<- as.numeric(gsub("[^]:digit:]]","",bronx1$`YEAR BUILT`)) 
bronx1$ZIP.CODE<- as.character(gsub("[^]:digit:]]","",bronx1$`ZIP CODE`)) 

minprice<-10000
bronx1<-bronx1[which(bronx1$SALE.PRICE>=minprice),]
nval<-dim(bronx1)[1]

bronx1$ADDRESSONLY<- gsub("[,][[:print:]]*","",gsub("[ ]+","",trim(bronx1$ADDRESS))) 
bronxadd<-unique(data.frame(bronx1$ADDRESSONLY, bronx1$ZIP.CODE,stringsAsFactors=FALSE)) 
names(bronxadd)<-c("ADDRESSONLY","ZIP.CODE") 
bronxadd<-bronxadd[order(bronxadd$ADDRESSONLY),] 
duplicates<-duplicated(bronx1$ADDRESSONLY)

for(i in 1:2345) {
  if(duplicates[i]==FALSE) dupadd<-bronxadd[bronxadd$duplicates,1]
}#what are we doing with dupadd?

nsample=450

addsample<-bronxadd[sample.int(dim(bronxadd),size=nsample),]#I use nval here 
# may need to install this package
library(ggmap)
register_google(key=)
addrlist<-paste(addsample$ADDRESSONLY, "NY", addsample$ZIP.CODE, "US", sep=" ") 
querylist<-geocode(addrlist) #This is cool. Take a break.

matched<-(querylist$lat!=0 &&querylist$lon!=0) 
addsample<-cbind(addsample,querylist$lat,querylist$lon) 
names(addsample)<-c("ADDRESSONLY","ZIPCODE","Latitude","Longitude")# correct the column na adduse<-merge(bronx1,addsample)

adduse<-adduse[!is.na(adduse$Latitude),]
mapcoord<-adduse[,c(2,3,24,25)]

table(mapcoord$NEIGHBORHOOD)

mapcoord$NEIGHBORHOOD <- as.factor(mapcoord$NEIGHBORHOOD)
map <- get_map(location = 'Bronx', zoom = 12)#Zoom 11 or 12
ggmap(map) + geom_point(aes(x = mapcoord$Longitude, y = mapcoord$Latitude, size =1, color=mapcoord$NEIGHBORHOOD), data = mapcoord) +theme(legend.position = "none") 

#It would be perfect if I can decrease the size of points 

mapmeans<-cbind(adduse,as.numeric(mapcoord$NEIGHBORHOOD))
colnames(mapmeans)[26] <- "NEIGHBORHOOD" #This is the right way of renaming.

keeps <- c("ZIP.CODE","NEIGHBORHOOD","TOTAL.UNITS","LAND.SQUARE.FEET","GROSS.SQUARE.FEET","SALE.PRICE","Latitude","Longitude") 
mapmeans<-mapmeans[keeps]#Dropping others
mapmeans$NEIGHBORHOOD<-as.numeric(mapcoord$NEIGHBORHOOD) 

for(i in 1:8){
  mapmeans[,i]=as.numeric(mapmeans[,i]) 
}#Now done for conversion to numeric

#Classification
mapcoord$class<as.numeric(mapcoord$NEIGHBORHOOD)
nclass<-dim(mapcoord)[1]
split<-0.8
trainid<-sample.int(nclass,floor(split*nclass))
testid<-(1:nclass)[-trainid]

##mappred<-mapcoord[testid,] # What would you use this for?
##mappred$class<as.numeric(mappred$NEIGHBORHOOD) 

kmax<-10
knnpred<-matrix(NA,ncol=kmax,nrow=length(testid))
knntesterr<-rep(NA,times=kmax)
for (i in 1:kmax){		# loop over k
  knnpred[,i]<-knn(mapcoord[trainid,3:4],mapcoord[testid,3:4],cl=mapcoord[trainid,2],k=i)
  knntesterr[i]<-sum(knnpred[,i]!=mapcoord[testid,2])/length(testid)
} 
knntesterr

#Clustering
mapobj<-kmeans(mapmeans,5, iter.max=10, nstart=5, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobj,method=c("centers","classes"))
mapobj$centers
#
library(cluster)
clusplot(mapmeans, mapobj$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
#
library(fpc)#May need to install.packages("fpc")
plotcluster(mapmeans, mapobj$cluster)
#
mapmeans1<-mapmeans[,-c(1,3,4)]
mapobjnew<-kmeans(mapmeans1,5, iter.max=10, nstart=5, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobjnew,method=c("centers","classes"))
clusplot(mapmeans1, mapobjnew$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
plotcluster(mapmeans1, mapobjnew$cluster)
ggmap(map) + geom_point(aes(x = mapcoord$Longitude, y = mapcoord$Latitude, size =1, color=mapobjnew$cluster), data = mapcoord)#How to change colors?

###Lab2




###Lab3



##group 2 lab 3 ctree1

require(rpart)
swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart) # try some different plot options
text(swiss_rpart) # try some different text options

require(party)

treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))
# look at help info, vary parameters.

library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)

##group 2 lab 3 ctree2
# Conditional Inference Tree for Mileage
fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
# plot tree
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")
text(fit2M, use.n=TRUE, all=TRUE, cex=.8)

##group 2 lab 3 ctree3
fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")

#etc.


##lab 1 ctree 2
pairs(~ Fertility + Education + Catholic, data = swiss, subset = Education < 20, main = "Swiss data, Education < 20")
require(party)
swiss_ctree <- ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_ctree)
# kknn1,
require(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, 
              prob = rep(1/m, m)) 
iris.learn <- iris[-val,]
iris.valid <- iris[val,]
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
                  kernel = "triangular")
summary(iris.kknn)
fit <- fitted(iris.kknn)
table(iris.valid$Species, fit)
pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red3")[(iris.valid$Species != fit)+1])

#, knn2, 
require(kknn)
data(ionosphere)
ionosphere.learn <- ionosphere[1:200,]
ionosphere.valid <- ionosphere[-c(1:200),]
fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
table(ionosphere.valid$class, fit.kknn$fit)
(fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
(fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)

##knn3
data(swiss)

pairs(~ Fertility + Education + Catholic, data = swiss, subset = Education < 20, main = "Swiss data, Education < 20")

#, kmeans1,
data(swiss)
sclass <- kmeans(swiss[2:6], 3) 
table(sclass$cluster, swiss[,1])    

#nyt

nyt1<-read.csv("C:/Users/phoen/Dropbox/MGMT 6962/nyt datasets/nyt1.csv")
nyt1<-nyt1[which(nyt1$Impressions>0 & nyt1$Clicks>0 & nyt1$Age>0),]
nnyt1<-dim(nyt1)[1]		# shrink it down!
sampling.rate=0.9
num.test.set.labels=nnyt1*(1.-sampling.rate)
training <-sample(1:nnyt1,sampling.rate*nnyt1, replace=FALSE)
train<-subset(nyt1[training,],select=c(Age,Impressions))
testing<-setdiff(1:nnyt1,training)
test<-subset(nyt1[testing,],select=c(Age,Impressions))
cg<-nyt1$Gender[training]
true.labels<-nyt1$Gender[testing]
classif<-knn(train,test,cg,k=5) #
classif
attributes(.Last.value) 
###
