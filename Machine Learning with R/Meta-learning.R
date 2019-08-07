#Bagging
library(ipred)
credit<-read.csv("credit.csv")
set.seed(300)
mybag<-bagging(as.factor(default)~., data=credit, nbagg=25)
credit_pred<-predict(mybag, credit)
table(credit_pred, credit$default)
#Plus 10-fold cv
library(caret)
set.seed(300)
ctrl<-trainControl(method="cv",number=10)
train(as.factor(default)~., data=credit, method="treebag",trContrl=ctrl)
#Bagging with SVM
library(kernlab)
str(svmBag)
bagctrl<-bagControl(fit=svmBag$fit,
                    predict = svmBag$pred,
                    aggregate = svmBag$aggregate)
set.seed(300)
svmbag<-train(as.factor(default)~., data=credit, "bag",trControl=ctrl,bagControl=bagctrl)
svmbag

#Boosting
library(adabag)
set.seed(300)
m_adaboost<-boosting(default~., data=credit)
#Adaboost with cv
set.seed(300)
adaboost_cv<-boosting.cv(default~., data=credit)

#Random Forests
library(randomForest)
set.seed(300)
rf<-randomForest(as.factor(default)~., data=credit)
rf

#Evaluating random forest performance
library(caret)
ctrl<-trainControl(method="repeatedcv", number=10, repeats=10)
grid_rf<-expand.grid(mtry=c(2,4,8,16))
set.seed(300)
m_rf<-train(as.factor(default)~., data=credit, method="rf", metric="Kappa", trControl=ctrl, tunegrid=grid_rf)

