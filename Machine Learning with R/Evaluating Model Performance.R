#Import dataset
credit<-read.csv("credit.csv")
#Holdout Methods
random_ids<-order(runif(1000))
credit_train<-credit[random_ids[1:500],]
credit_validate<-credit[random_ids[501:750],]
credit_test<-credit[random_ids[751:1000],]

#Stratified Random Sampleing
in_train<-createDataPartition(credit$default, p=0.75, list=FALSE)
credit_train<-credit[in_train,]
credit_test<-credit[-in_train,]

#Cross-validation
library(caret)
folds<-createFolds(credit$default, k=10)
str(folds)
credit01_test<-credit[folds$Fold01,]
credit01_train<-credit[-folds$Fold01,]#Require to repeat 10 times manually
#Automatic process
library(caret)
library(C50)
library(irr)
cv_results<-lapply(folds, function(x){
  credit_train<-credit[-x,]
  credit_test<-credit[x,]
  credit_model<-C5.0(as.factor(default)~., data=credit_train)
  credit_pred<-predict(credit_model, credit_test)
  credit_actual<-credit_test$default
  kappa<-kappa2(data.frame(credit_actual, credit_pred))$value
})
str(cv_results)
#Calculate the average
mean(unlist(cv_results))



