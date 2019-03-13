#Load libraries
library(randomForest)
library(gbm)
library(caret)
library(Ecdat)

#Import data
data("Participation")
str(Participation)

#Data preprocessing
Participation$age<-10*Participation$age
Participation$lnnlinc<-exp(Participation$lnnlinc)

#Create training and testing sets
set.seed(1234)
ind=sample(2, nrow(Participation), replace=T, prob = c(.7,.3))
train<-Participation[ind==1,]
test<-Participation[ind==2,]

#Grid and Control
grid<-expand.grid(.n.trees=seq(200,500,by=200), .interaction.depth=seq(1,3,by=2), .shrinkage=seq(.01,.09,by=.04), .n.minobsinnode=seq(1,5,by=2) )
control<-trainControl(method="cv", number=10)

#Parameter Selection
set.seed(123)
gbm.lfp.train<-train(lfp~., data=train, method="gbm", trControl=control, tuneGrid=grid)
gbm.lfp.train

#Model Training
train$lfp=ifelse(train$lfp=="no",0,1)
gbm.lfp<-gbm(lfp~., distribution = "bernoulli",data=train, n.trees=400,interaction.depth = 1, shrinkage = .01, n.minobsinnode = 3)
summary(gbm.lfp)

#Model testing
gbm.lfp.test<-predict(gbm.lfp, newdata=test, type='response', n.trees=400)
gbm.class<-ifelse(gbm.lfp.test<-.5, 'no','yes')
table(gbm.class, test$lfp)
