#Load the packages
library(mlr)
library(tidyverse)

#Load dataset
library(titanic)
data("titanic_train")
titanicTib<-as_tibble(titanic_train)
titanicTib

#Cleaning the Titanic data ready for modeling
fctrs<-c("Survived", "Sex", "Pclass")
titanicClean<-titanicTib%>%
  mutate_at(.vars=fctrs, .funs=factor)%>%
  mutate(FamSize=SibSp+Parch)%>%
  select(Survived, Pclass, Sex, Age, Fare, FamSize)

#Creating an untidy tibble for plotting
titanicUntidy<-gather(titanicClean, key="Variable", value="Value",
                      -Survived)
titanicUntidy

#Create subplots for each continuous variable
titanicUntidy%>%
  filter(Variable != "Plclss" & Variable != "Sex")%>%
  ggplot(aes(Survived, as.numeric(Value)))+
  facet_wrap(~Variable, scales="free_y")+
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75))+
  theme_bw()

#Training a tasker and learner, and training a model
titanicTask<-makeClassifTask(data=titanicClean, target="Survived")
logReg<-makeLearner("classif.logreg")
logRegModel<-train(logReg, titanicTask)  

#Imputing missing values in the age Variable
imp<-impute(titanicClean, cols=list(Age=imputeMean()))
sum(is.na(titanicClean$Age))
sum(is.na(imp$data$Age))

#Training the model
titanicTask<-makeClassifTask(data=imp$data, target="Survived")
logRegModel<-train(logReg, titanicTask)

#Wrapping together the learner and the imputation method
logRegWrapper<-makeImputeWrapper("classif.logreg",
                                 cols=list(Age=imputeMean()))

#Cross validating model building process
kFold<-makeResampleDesc(method="RepCV", folds=10, reps=50, stratify = TRUE)
logRegwithImpute<-resample(logRegWrapper, titanicTask,
                           resampling = kFold,
                           measures=list(acc, fpr, fnr))
logRegwithImpute

#Extracting model parameters
logRegModelData<-getLearnerModel(logRegModel)
coef(logRegModelData)

#Converting model parameters into odds ratios
exp(cbind(Odds_Ratio=coef(logRegModelData), confint(logRegModelData)))

#Using model to make predictions on new data
data("titanic_test", package = "titanic")
titanicNew<-as.tibble(titanic_test)
titanicNewClean<-titanicNew%>%
  mutate_at(.vars=c("Sex","Pclass"), .funs=factor)%>%
  mutate(FamSize=SibSp+Parch)%>%
  select(Pclass, Sex, Age, Fare, FamSize)
predict(logRegModel, newdata = titanicNewClean)
