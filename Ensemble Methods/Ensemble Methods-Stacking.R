#Stacking
#Building multiple models (typically of differing types) and supervisor model that learns how to best combine the predictions of the primary models

#Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)

#Load the dataset
data("Ionosphere")
dataset<-Ionosphere
head(dataset)
dim(dataset)

dataset=dataset[,-2]
dim(dataset)
dataset$V1<-as.numeric(as.character(dataset$V1))

control<-trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE,algorithmList<-c('lda','rpart','glm','knn','svmRadial'), classProbs=TRUE)
set.seed(seed)
models<-caretList(Class~., data=dataset, trControl=control, methodList=algorithmList)
results<-resamples(models)
summary(results)
dotplot(results)

#Correlation between results
modelCor(results)
splom(results)

#Stack using glm
stackControl<-trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.glm<-caretStack(models, method="glm", metric="Accuracy",trControl=stackControl)
print(stack.glm)

#Stack using random forest
set.seed(seed)
stack.rf<-caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)
