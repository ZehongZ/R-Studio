#Load packages
library(mlr)
library(tidyverse)

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

#Plotting the data
ozoneUntidy<-gather(ozoneClean, key="Variable",
                    value="Value",-Ozone)
ozoneUntidy
ggplot(ozoneUntidy, aes(Value, Ozone))+
  facet_wrap(~Variable, scale="free_x")+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm", col="red")+
  theme_bw()

#Using rpart to impute missing values
imputeMethod<-imputeLearner("regr.rpart")
ozoneImp<-impute(as.data.frame(ozoneClean),
                 classes=list(numeric=imputeMethod))

#Defining our task and learner
ozoneTask<-makeRegrTask(data=ozoneImp$data, target = "Ozone")
lin<-makeLearner("regr.lm")

#Using a filter method for feature selection
filterVals<-generateFilterValuesData(ozoneTask, method="linear.correlation")
filterVals$data

#Creating a filter wrapper
filterWrapper=makeFilterWrapper(learner = lin,
                                fw.method = "linear.correlation")

#Tuning the number of predictors to retain
lmParamSpace<-makeParamSet(
  makeIntegerParam("fw.abs", lower=1, upper = 12)
)
gridSearch<-makeTuneControlGrid()
kFold<-makeResampleDesc("CV", iters=10)
tunedFeats<-tuneParams(filterWrapper, task=ozoneTask, resampling = kFold,
                      par.set = lmParamSpace, control=gridSearch)
tunedFeats

#Train model with filtered features
filteredTask<-filterFeatures(ozoneTask, fval = filterVals,
                             abs=unlist(tunedFeats$x))
filteredModel<-train(lin, filteredTask)

#Using a wrapper method for feature selection
featSelControl<-makeFeatSelControlSequential(method="sfbs")
selFeats<-selectFeatures(learner=lin, task=ozoneTask,
                         resampling=kFold, control=featSelControl)
selFeats

#Using a wrapper method for feature selection
ozoneSelFeat<-ozoneImp$data[,c("Ozone", selFeats$x)]
ozoneSelFeatTask<-makeRegrTask(data=ozoneSelFeat, target="Ozone")
wrapperModel<-train(lin, ozoneSelFeatTask)

#Combining imputation and feature selection wrappers
imputeMethod<-imputeLearner("regr.rpart")
imputeWrapper<-makeImputeWrapper(lin, classes=list(numeric=imputeMethod))
featSelWrapper<-makeFeatSelWrapper(learner = imputeWrapper,
                                   resampling = kFold,
                                   control=featSelControl)

#Cross-validating the model building process
library(parallel)
library(parallelMap)
ozoneTaskWithNAs<-makeRegrTask(data=ozoneClean, target ="Ozone")
kFold3<-makeResampleDesc("CV",iters=3)
parallelStartSocket(cpus = detectCores())
lmCV<-resample(featSelWrapper, ozoneTaskWithNAs, resampling = kFold3)

#Interpreting the model
wrapperModelData<-getLearnerModel(wrapperModel)
summary(wrapperModelData)

#Creating diagnostic plots of the model
par(mfrow=c(2,2))
plot(wrapperModelData)
par(mfrow=c(1,1))


