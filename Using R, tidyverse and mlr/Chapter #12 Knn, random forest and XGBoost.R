##First Knn
#Load packages
library(mlr)
library(tidyverse)

#Loading and exploring dataset
data("fuelsubset.task")
fuel<-getTaskData(fuelsubset.task)
fuelTib<-as_tibble(fuel)
fuelTib

#Preparing the data for plotting
fuelUntidy<-fuelTib%>%
  mutate(id=1:nrow(.))%>%
  gather(key="variable",value = "absorbance",
         c(-heatan, -h20, -id))%>%
  mutate(spectrum=str_sub(variable, 1,3),
         wavelength=as.numeric(str_extract(variable, "(\\d)+")))
fuelUntidy

#Plotting the data
fuelUntidy%>%
  ggplot(aes(absorbance, heatan, col=as.factor(wavelength)))+
  facet_wrap(~spectrum, scales="free_x")+
  geom_smooth(se=FALSE, size=0.2)+
  ggtitle("Absorbance vs heatan for each wavelength")+
  theme_bw()+
  theme(legend.position = "none")

fuelUntidy%>%
  ggplot(aes(wavelength, absorbance, group=id, col=heatan))+
  facet_wrap(~spectrum, scales="free_x")+
  geom_smooth(se=FALSE, size=0.2)+
  ggtitle("Wavelength vs absorbance for each batch")+
  theme_bw()

fuelUntidy%>%
  ggplot(aes(h20, heatan))+
  geom_smooth(se=FALSE)+
  ggtitle("Humidity vs heatan")+
  theme_bw()

#Defining the task and k-NN learner
fuelTask<-makeRegrTask(data=fuelTib, target ="heatan")
library(kknn)
kknn<-makeLearner("regr.kknn")

#Tuning the k hyperparameter
kknnParamSpace<-makeParamSet(makeDiscreteParam("k", values=1:12))
gridSearch<-makeTuneControlGrid()
kFold<-makeResampleDesc("CV", iters=10)
tunedK<-tuneParams(kknn, task=fuelTask,
                   resampling = kFold,
                   par.set=kknnParamSpace,
                   control=gridSearch)
tunedK

#Plotting the tuning process
knnTuningData<-generateHyperParsEffectData(tunedK)
plotHyperParsEffect(knnTuningData, x="k", y="mse.test.mean",
                    plot.type="line")+
  theme_bw()

#Training the final, tuned k-NN model
tunedKnn<-setHyperPars(makeLearner("regr.kknn"), par.vals=tunedK$x)
tunedKnnModel<-train(tunedKnn, fuelTask)


##Random Forest regression model
library(randomForest)
forest<-makeLearner("regr.randomForest")

#Hyperparameter tuning for random forest
forestParamSpace<-makeParamSet(
  makeIntegerParam("ntree", lower=50, upper=50),
  makeIntegerParam("mtry", lower=100, upper=367),
  makeIntegerParam("nodesize",lower=1, upper=10),
  makeIntegerParam("maxnodes", lower=5, upper=30)
)
randSearch<-makeTuneControlRandom(maxit=100)
library(parallel)
library(parallelMap)
parallelStartSocket(cpus=detectCores())
tunedForestPars<-tuneParams(forest, task=fuelTask,
                            resampling=kFold,
                            par.set=forestParamSpace,
                            control=randSearch)
parallelStop()
tunedForestPars

#Training the random forest model and plotting the out-of-bag error
tunedForest<-setHyperPars(forest, par.vals=tunedForestPars$x)
tunedForestModel<-train(tunedForest, fuelTask)
forestModelData<-getLearnerModel(tunedForestModel)
plot(forestModelData)

##Building first XGBoost regression model
xgb<-makeLearner("regr.xgboost")

#Hyperparameter turning for XGBoost
xgbParamSpace<-makeParamSet(
  makeNumericParam("eta", lower=0, upper=1),
  makeNumericParam("gamma", lower=0, upper = 10),
  makeIntegerParam("max_depth", lower=1, upper=20),
  makeNumericParam("min_child_weight",lower=1,upper=10),
  makeNumericParam("subsample", lower=0.5,upper=1),
  makeNumericParam("colsample_bytree",lower=0.5, upper=1),
  makeIntegerParam("nrounds", lower=30, upper=30)
)
tunedXgbPars<-tuneParams(xgb, task=fuelTask,
                         resampling=kFold,
                         par.set=xgbParamSpace,
                         control = randSearch)
tunedXgbPars

#Training the XGBoost model and plotting the RMSE against number
tunedXgb<-setHyperPars(xgb, par.vals=tunedXgbPars$x)
tunedXgbModel<-train(tunedXgb, fuelTask)
xgbModelData<-getLearnerModel(tunedXgbModel)
ggplot(xgbModelData$evaluation_log, aes(iter, train_rmse))+
  geom_line()+
  geom_point()+
  theme_bw()

#Benchmarking k-NN, random forest, and XGBoost
kknnWrapper<-makeTuneWrapper(kknn, resampling = kFold,
                             par.set = kknnParamSpace,
                             control=gridSearch)
forestWrapper<-makeTuneWrapper(forest, resampling = kFold,
                               par.set=forestParamSpace,
                               control=randSearch)
xgbWrapper<-makeTuneWrapper(xgb, resampling=kFold,
                            par.set=xgbParamSpace,
                            control=randSearch)
learners=list(kknnWrapper, forestWrapper, xgbWrapper)
holdout<-makeResampleDesc("Holdout")
bench<-benchmark(learners, fuelTask,holdout)
bench
