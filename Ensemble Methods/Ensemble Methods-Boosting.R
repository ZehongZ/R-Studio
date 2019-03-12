#Boosting
#Building multiple models(typically of the same type) each of which learns to fix the prediction errors of a prior model in the chain

#Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)

#Load the dataset
data("Ionosphere")
dataset<-Ionosphere

head(dataset)
dataset<-dataset[,-2]
head(dataset)
dataset$V1<-as.numeric(as.character(dataset$V1))

#Boosting algorithm
control<-trainControl(method="repeatedcv", number=10, repeats=3)
seed<-7
metric<-"Accuracy"

#C5
set.seed(seed)
fit.c50<-train(Class~., data=dataset, method="C5.0", metric=metric, trControl=control)

#Stochastic Gradient Boosting
set.seed(seed)
fit.gbm<-train(Class~., data=dataset, method="gbm",metric=metric, trControl=trainControl)

#Summarize results
boosting_results<-resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)


#