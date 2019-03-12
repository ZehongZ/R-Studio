#Bagging: building multiple models (typically of the same type) from differnt subsamples of the training dataset

#Loading libraries
library(psych)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(partykit)
library(pROC)
library(ISLR)

#Import data
data("Carseats")

#Understanding dataset
head(Carseats)
describe(Carseats)

#Histogram of outcome
ggplot(data=Carseats, aes(x=Sales))+geom_histogram(binwidth = 1, boundary=.5, fill="white",color="black")+geom_vline(xintercept = 8, color="red",size=2)+labs(x="Sales")

#Creating new binary variable 
Carseats$HighSales=ifelse(Carseats$Sales<=8, "No","Yes")

#Remove old variable
Carseats$Sales<-NULL

#Convert a factor variable into a numeric variable 
Carseats$ShelveLoc<-as.numeric(Carseats$ShelveLoc)

#Splitting the data into training and test sets
halfsample=sample(dim(Carseats)[1], dim(Carseats)[1]/2)
Carseats.train=Carseats[halfsample,]
Carseats.test=Carseats[-halfsample,]

#Setting up the k-fold cross validation k=10 cross-validation folds
set.seed(1234)
cvcontrol<-trainControl(method="repeatedcv",number=10, allowParallel=TRUE)

#A single classification Tree
train.tree<-train(as.factor(HighSales)~., data=Carseats.train, method="ctree",trControl=cvcontrol, tuneLength=10)
train.tree
plot(train.tree)

#Plot tree
plot(train.tree$finalModel, main="Regression Tree for Carseat High Sales")

#Obtaining class prediction
tree.classTrain<-predict(train.tree, type="raw")
head(tree.classTrain)

#Computing confusion matrix
confusionMatrix(Carseats.train$HighSales, tree.classTrain)

#Obtaining class predictions
tree.ClassTest<-predict(train.tree, newdata = Carseats.test, type="raw")
head(tree.ClassTest)

#Computing confusion matrix
confusionMatrix(Carseats.train$HighSales, tree.ClassTest)

#Obtaining predicted probabilities for test data
tree.probs=predict(train.tree, newdata=Carseats.test, type="prob")
head(tree.probs)

#Calculate ROC curve
rocCurve.tree<-roc(Carseats.test$HighSales, tree.probs[,"Yes"])
plot(rocCurve.tree,col=c(4))

#Calculate the area under curve
auc(rocCurve.tree)

#Bagging of ctrees
train.bagg<-train(as.factor(HighSales)~., data=Carseats.train, method="treebag",trControl=cvcontrol, importance=TRUE)
train.bagg
plot(varImp(train.bagg))

#Obtaining class predictions
bagg.classTrain<-predict(train.bagg,type="raw")
head(bagg.classTrain)

#Computing confusion matrix
confusionMatrix(Carseats.train$HighSales, bagg.classTrain)

#Obtaining class predictions
bagg.classTest<-predict(train.bagg, newdata = Carseats.test, type="raw")
head(bagg.classTest)

#Computing confusion matrix
confusionMatrix(Carseats.test$HighSales, bagg.classTest)

#Obtaining predicted probabilites for Test data
bagg.probs=predict(train.bagg, newdata = Carseats.test, type="prob")
head(bagg.probs)

#Calculate ROC curve
rocCurve.bagg<-roc(Carseats.test$HighSales, bagg.probs[,"Yes"])
plot(rocCurve.bagg, col=c(6))

#Calculate the area under curve 
auc(rocCurve.bagg)








#Training the model using random forest
train.rf<-train(as.factor(HighSales)~., data=Carseats.train, method="rf", trControl=cvcontrol, importance=TRUE)
train.rf

#Obtaining class predictions
rf.classTrain<-predict(train.rf, type="raw")
head(rf.classTrain)

#Computing confusion matrix
confusionMatrix(Carseats.train$HighSales, rf.classTrain)

#Obtaining class predictions
rf.classTest<-predict(train.rf, newdata=Carseats.test, type="raw")
head(rf.classTest)

#Computing confusion matrix
confusionMatrix(Carseats.test$HighSales, rf.classTest)

#Obtaining predicted porbabilities for test data
rf.probs=predict(train.rf, newdata=Carseats.test, type="prob")
head(rf.probs)

#Calculate ROC curve
rocCurve.rf<-roc(Carseats.test$HighSales, rf.probs[,"Yes"])

#Plot the ROC curve
plot(rocCurve.rf,col=c(1))

#Calculate the area under curve
auc(rocCurve.rf)





#CForest for conditional inference tree
train.cf<-train(HighSales~., data=Carseats.train, method="cforest",trControl=cvcontrol)
train.cf

#Obtaining class predictions
cf.classTrain<-predict(train.cf, type="raw")
head(cf.classTrain)

#Computing confusion matrix
confusionMatrix(Carseats.train$HighSales, cf.classTrain)

#Obtaining class predictions
cf.classTest<-predict(train.cf, newdata = Carseats.test, type="raw")
head(cf.classTest)

#Computing confusion matrix
confusionMatrix(Carseats.test$HighSales, cf.classTest)

#Ontaining predicted probabilites for test data
cf.probs=predict(train.cf, newdata=Carseats.test, type="prob")
head(cf.probs)

#Calculate ROC curve
rocCurve.cf<-roc(Carseats.test$HighSales, cf.probs[,"Yes"])

#Plot the ROC curve
plot(rocCurve.cf, col=c(2))

#Calculate the area under curve
auc(rocCurve.cf)




#Random Forest with Boosting
modelLookup("ada")
modelLookup("gbm")

#Training with gradient boosting
train.gbm<-train(as.factor(HighSales)~., data=Carseats.train, method="gbm",verbose=F, trControl=cvcontrol)
train.gbm

#Obtaining class predictions
gbm.classTrain<-predict(train.gbm, type="raw")
head(gbm.classTrain)

#Computing confusion matrix
confusionMatrix(Carseats.train$HighSales, gbm.classTrain)

#Obtaining predicted probabilites for test data
gbm.probs=predict(train.gbm, newdata=Carseats.test, type="prob")
head(gbm.probs)

#Calculate ROC curve
rocCurve.gbm<-roc(Carseats.test$HighSales, gbm.probs[,"Yes"])

#Plot the ROC curve
plot(rocCurve.gbm, col=c(3))

#Claculate the area under curve
auc(rocCurve.gbm)
