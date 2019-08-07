#Holdout
library(caret)
library(klaR)
data("iris")
trainIndex=createDataPartition(iris$Species, p=0.8, list=FALSE)
data_train=iris[trainIndex,]
data_test=iris[-trainIndex,]
fit=NaiveBayes(Species~., data=data_train)
predictions=predict(fit, data_test[1:4])
confusionMatrix(predictions$class, data_test$Species)

#Bootstrap
library(caret)
data("iris")
trainControl=trainControl(method="boot", number=100)
fit=train(Species~., data=iris, trControl=trainControl, method="nb")

#Repeated k-fold Cross_validataion
library(caret)
data("iris")
trainControl=trainControl(method="repeatedcv", number=10, repeats=3)
fit=train(Species~., data=iris, trControl=trainControl, method="nb")
print(fit)

#Leave-one-out Cross-validation
library(caret)
data("iris")
trainControl=trainControl(method="LOOCV")
fit=train(Species~., data=iris, trControl=trainControl, method="nb")
print(fit)
