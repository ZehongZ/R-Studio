#Identifying missing values
library(VIM)
data(sleep)

#List the rows that do not have missing values
sleep[complete.cases(sleep),]

#List the rows that have one or more missing values
sleep[!complete.cases(sleep),]

sum(is.na(sleep$Dream))
mean(is.na(sleep$Dream))
mean(!complete.cases(sleep))

#Tabulating missing values
library(mice)
library(VIM)
data(sleep)
md.pattern(sleep)

#Exploring missing data visually
library(VIM)
aggr(sleep, prop=FALSE, numbers=TRUE)

##Matrixplot() function produces a plot displaying the data for each case
matrixplot(sleep)

##The marginplot() function produces a scatter plot between two vairables with information about missing values shown in the plot's margins
marginplot(sleep[c("Gest","Dream")], pch=c(20), col=c("darkgray","red","blue"))

#Using correlations to explore missing values
x<-as.data.frame(abs(is.na(sleep)))
head(sleep, n=5)
head(x, n=5)
y<-x[which(apply(x,2,sum)>0)]
cor(y)
cor(sleep, y, use="pairwise.complete.obs")

#Complete-case analysis (listwise deletion)
data(sleep)
options(digits=1)
cor(na.omit(sleep))
fit<-lm(Dream~Span+Gest, data=na.omit(sleep))
summary(fit)

#Multiple imputation
library(mice)
library(VIM)
data("sleep")
imp<-mice(sleep, seed=1234)
fit<-with(imp, lm(Dream~Span+Gest))
pooled<-pool(fit)
summary(pooled)
imp
imp$imp$Dream
dataset3<-complete(imp, action=3)
dataset3

#Pairwise deletion
cor(sleep, use="pairwise.complete.obs")
