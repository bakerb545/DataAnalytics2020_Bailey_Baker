rm(ist=ls())

require(ggplot2)
data(diamonds)
head(diamonds)

ggplot(diamonds, aes(clarity, fill=cut))+geom_bar()
ggplot(diamonds, aes(clarity))+geom_bar()+facet_wrap(~cut)
ggplot(diamonds)+geom_histogram(aes(x=price))+geom_vline(xintercept=12000)

ggplot(data=diamonds, mapping=aes(color=cut_number(carat,5), x=price))+
  geom_freqpoly()+labs(x="Price", y="Count", color="Carat")


ggplot(diamonds, aes(x=cut_number(price,10), y=carat))+
  geom_boxplot() + coord_flip()+xlab("Price")

ggplot(diamonds, aes(x=cut_number(carat,5), y=price, colour=cut))+
  geom_boxplot()
library(MASS)
library(ISLR)
help(lda)
data("Smarket")
attach(Smarket)
head(Smarket)

names(Smarket)
str(Smarket)
dim(Smarket)

train<- (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005<- Direction[!train]

lda.fit<- lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit

lda.pred<- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class<- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class== Direction.2005)
