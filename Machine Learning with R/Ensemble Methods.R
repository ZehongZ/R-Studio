#Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)

#Import dataset
data("Ionosphere")
dataset<-Ionosphere
dataset<-dataset[,-2]
dataset$V1<-as.numeric(as.character(dataset$V1))

#Example of the dataset
head(dataset)
str(dataset)

###Boosting Algorithm###
library(C50)
control<-trainControl(method="repeatedcv", number=10, repeats=3)
seed<-7
metric<-"Accuracy"
#C5.0
set.seed(seed)
fit.c50<-train(Class~., data=dataset, method="C5.0", metric=metric, trControl=control)
#Stochastic Gradient Boosting
set.seed(seed)
fit.gbm<-train(Class~., data=dataset, method="gbm", metric=metric, trControl=control)
#Summarize results
boosting_results<-resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)

###Bagging Algorithms###
control<-trainControl(method="repeatedcv", number=10, repeats=3)
seed<-7
metric<-"Accuracy"
#Bagged CART
set.seed(seed)
fit.treebag<-train(Class~., data=dataset, method="treebag", metric=metric, trControl=control)
#Random Forest
set.seed(seed)
fit.rf<-train(Class~., data=dataset, method="rf", metric=metric, trControl=control)
#Summarize results
bagging_results<-resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)

###Stacking Algorithms###
control<-trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProb=TRUE)
algorithmList<-c('lda','rpart','glm','knn','svmRadial')
set.seed(seed)
models<-caretList(Class~., data=dataset, trControl = control, methodList = algorithmList)
results<-resamples(models)
summary(results)
dotplot(results)
#Correlation between results
modelCor(results)
splom(results)
#Stack using glm
stackControl<-trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE,classProb=TRUE)
set.seed(seed)
stack.glm<-caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)
#Stack using random forest
set.seed(seed)
stack.rf<-caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)
