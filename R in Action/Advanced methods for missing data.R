#Identifying missing values
#Load the dataset
data(sleep, package = "VIM")
#List the rows that do not have missing values
sleep[complete.cases(sleep),]
#List the rows that have one or more missing values
sleep[!complete.cases(sleep),]

sum(is.na(sleep$Dream))
mean(is.na(sleep$Dream))
mean(!complete.cases(sleep))

#Tabulating missing values
library(mice)
data(sleep, package = "VIM")
md.pattern(sleep)

#Exploring missing data visually
library(VIM)
aggr(sleep, prop=FALSE, numbers=TRUE)

#Marginplot
marginplot(sleep[c("Gest","Dream")], pch=c(20),
           col=c("darkgrey","red","blue"))

#Using correlations to explore missing value
x<-as.data.frame(abs(is.na(sleep)))
head(sleep,n=5)
head(x,n=5)
y<-x[which(apply(x,2,sum)>0)]
cor(sleep, y, use="pairwise.complete.obs")

#Multiple imputation
library(mice)
data(sleep, package = "VIM")
imp<-mice(sleep, seed=1234)
fit<-with(imp, lm(Dream~Span+Gest))
pooled<-pool(fit)
summary(pooled)
imp
imp$imp$Dream
complete(imp, action=#)

#pairwise deletion
  cor(sleep, use="pairwise.complete.obs")
