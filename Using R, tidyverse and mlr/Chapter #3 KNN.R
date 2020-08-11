#Build KNN model
library(mlr)
library(tidyverse)

#Loading the diabetes data
data(diabetes, package="mclust")
diabetesTib<-as_tibble(diabetes)
summary(diabetesTib)

#Plotting the diabetes
library(ggplot2)
ggplot(diabetes, aes(x=glucose, y=insulin, col=class))+
  geom_point()+
  theme_bw()
ggplot(diabetesTib, aes(sspg, insulin, col=class))+
  geom_point()+
  theme_bw()
ggplot(diabetesTib, aes(sspg, glucose, col=class))+
  geom_point()+
  theme_bw()
diabetesTask<-makeClassifTask(data=diabetesTib, target = "class")
diabetesTask

#Defineing the learner
knn<-makeLearner("classif.knn", par.vals = list("k"=2))

#Training the model
knnModel<-train(knn, diabetesTask)
knnPred<-predict(knnModel, newdata=diabetesTib)
performance(knnPred, measures=list(mmce, acc))

#Creating a hldout cross validation resampling description
holdout<-makeResampleDesc(method="Holdout", split=2/3, stratify = TRUE)
#Performaing Hold-out Cross-validation
holdoutCV<-resample(learner=knn, task=diabetesTask,
                    resampling = holdout, measures=list(mmce, acc))
#Confusion matrix for hold-out cross-validation
calculateConfusionMatrix(holdoutCV$pred, relative=TRUE)

#Creating a k-fold cross validation resampling description
kFold<-makeResampleDesc(method="RepCV", folds=10, reps=50, stratify=TRUE)
kFoldCV<-resample(learner=knn, task=diabetesTask, resampling=kFold, measures=list(mmce,acc))
kFoldCV$aggr
#Calculating a confustion matrix
calculateConfusionMatrix(kFoldCV$pred, relative = TRUE)

#Creating a leave-one-out cross validation resampling description
LOO<-makeResampleDesc(method="LOO")
LOOCV<-resample(learner=knn, task=diabetesTask, resampling=LOO, 
                measures=list(mmce, acc))
LOOCV$aggr
#Calculating a confusion matrix
calculateConfusionMatrix(LOOCV$pred, relative=TRUE)

#Turning k to improve our model
knnParamSpace<-makeParamSet(makeDiscreteParam("k", values=1:10))
gridSearch<-makeTuneControlGrid()
cvForTuning<-makeResampleDesc("RepCV", folds=10, reps=20)
tunedK<-tuneParams("classif.knn", task=diabetesTask,
                   resampling = cvForTuning,
                   par.set=knnParamSpace, control=gridSearch)
tunedK
#Visualize the tuning process
knnTuningData<-generateHyperParsEffectData(tunedK)
plotHyperParsEffect(knnTuningData, x="k", y= "mmce.test.mean",
                    plot.type = "line")+
  theme_bw()
#Train for final model
tunedKnn<-setHyperPars(makeLearner('classif.knn'), par.vals=tunedK$x)
tunedModel<-train(tunedKnn, diabetesTask)

#Including hyperparameter tuning in our cross-validation
inner<-makeResampleDesc("CV")
outer<-makeResampleDesc("RepCV", folds=10, reps=5)
knnWrapper<-makeTuneWrapper("classif.knn", resampling=inner,
                            par.set=knnParamSpace,
                            control = gridSearch)
cvWithTuning<-resample(knnWrapper, diabetesTask, resampling = outer)
cvWithTuning

#Using model to make predictions
newDiabetesPatients<-tibble(glucose=c(82,108,300),
                            insulin=c(361, 288, 1052),
                            sspg=c(200, 186, 135))
newDiabetesPatients
newPatientsPred<-predict(tunedModel, newdata=newDiabetesPatients)
getPredictionResponse(newPatientsPred)
