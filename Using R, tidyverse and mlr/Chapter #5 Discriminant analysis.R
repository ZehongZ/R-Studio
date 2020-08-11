#Building first linear and quadratic discriminant models
library(mlr)
library(tidyverse)

#Loading and exploring the wine dataset
wine=read.csv("winedata.csv", header = TRUE)
head(wine)
wineTib<-as_tibble(wine)
wineTib

#Cleaning the dataset
names(wineTib)<-c("Class","Alco","Malic","Ash","Alk","Mag",
                  "Phe","Flav","Non_flav","Proan","Col","Hue","OD","Prol")
wineTib$Class<-as.factor(wineTib$Class)
wineTib

#Creating an untidy tibble for plotting
wineUntidy<-gather(wineTib,"Variable","Value", -Class)
wineUntidy
ggplot(wineUntidy, aes(Class, Value))+
  facet_wrap(~Variable, scales="free_y")+
  geom_boxplot()+
  theme_bw()

#Creating the task and learner, and training the model
wineTask<-makeClassifTask(data=wineTib, target="Class")
lda<-makeLearner("classif.lda")
ldaModel<-train(lda, wineTask)

#Extracting discriminant function values for each case
ldamodelData<-getLearnerModel(ldaModel)
ldaPreds<-predict(ldamodelData)$x
head(ldaPreds)

#Plotting the discriminant function values against each other
wineTib%>%
  mutate(LD1=ldaPreds[,1],
         LD2=ldaPreds[,2])%>%
  ggplot(aes(LD1, LD2, col=Class))+
  geom_point()+
  stat_ellipse()+
  theme_bw()

#Plotting the discriminant function values against each other
qda<-makeLearner("classif.qda")
qdaModel<-train(qda, wineTask)

#Cross-validating the LDA and QDA models
kFold<-makeResampleDesc(method="RepCV", folds=10, reps=50,
                        stratify=TRUE)
ldaCV<-resample(learner = lda, task=wineTask, resampling=kFold,
                measures=list(mmce, acc))
qdaCV<-resample(learner=qda, task=wineTask, resampling=kFold,
                measures=list(mmce, acc))
ldaCV$aggr
qdaCV$aggr

#Calculating confusion matrices
calculateConfusionMatrix(ldaCV$pred, relative=TRUE)
calculateConfusionMatrix(qdaCV$pred, relative=TRUE)

#Predicting which vineyard the poisoned wine came from
poisoned<-tibble(Alco=13, Malic=2, Ash=2.2, Alk=19, Mag=100,
                 Phe=2.3, Flav=2.5, Non_flav=0.35, Proan=1.7,
                 Col=4, Hue=1.1, OD=3, Prol=750)
predict(qdaModel, newdata=poisoned)
