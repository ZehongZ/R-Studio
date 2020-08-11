#Install packages
library(mlr)
library(tidyverse)
library(stabs)
library(mboost)

#Loading dataset
data(Ozone, package = "mlbench")
ozoneTib<-as_tibble(Ozone)
names(ozoneTib)
names(ozoneTib)<-c("Month","Date","Day","Ozone","Press_height",
                   "Wind","Humid","Temp_Sand","Temp_Monte",
                   "Inv_height","Press_grad","Inv_temp","Visib")
ozoneTib

#Cleaning the data
ozoneClean<-mutate_all(ozoneTib, as.numeric)%>%
  filter(is.na(Ozone)==FALSE)
ozoneClean

#Creating an interaction between Date and Month
ozoneForGam<-mutate(ozoneClean, DayofYear=as.numeric(interaction(Date, Month)))%>%
  select(c(-"Date",-"Month"))

#Defining the task, imputation and feature selection wrappers
gamTask<-makeRegrTask(data=ozoneForGam, target="Ozone")
imputedMethod<-imputeLearner("regr.rpart")
gamImputeWrapper<-makeImputeWrapper("regr.gamboost",
                                    classes=list(numeric=imputedMethod))
gamFeatSelControl<-makeFeatSelControlSequential(method="sfbs")
kFold<-makeResampleDesc("CV", iters=10)
gamFeatSelWrapper<-makeFeatSelWrapper(learner=gamImputeWrapper,
                                      resampling=kFold,
                                      control=gamFeatSelControl)

#Cross-validating the GAM model building process
holdout<-makeResampleDesc("Holdout")
gamCV<-resample(gamFeatSelWrapper, gamTask, resampling = holdout)
gamCV

#Training a GAM
library(parallel)
library(parallelMap)
parallelStartSocket(cpus=detectCores())
gamModel<-train(gamFeatSelWrapper,gamTask)
parallelStop()
gamModelData<-getLearnerModel(gamModel, more.unwrap = TRUE)

#Plotting our GAM
par(mfrow=c(3,3))
plot(gamModelData,type="l")
plot(gamModelData$fitted(), resid(gamModelData))
qqnorm(resid(gamModelData))
qqline(resid(gamModelData))
par(mfrow=c(1,3))
