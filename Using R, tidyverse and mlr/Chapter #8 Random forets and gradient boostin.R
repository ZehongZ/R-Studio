#First random forest model
library(mlr)
library(tidyverse)
forest<-makeLearner("classif.randomForest")

#Loading and exploring the zoo dataset
data(Zoo, package = "mlbench")
zooTib<-as_tibble(Zoo)
zooTib

#Converting logical variables to factors
zooTib<-mutate_if(zooTib, is.logical, as.factor)

#Creating the task and learner
zooTask<-makeClassifTask(data=zooTib, target = "type")

#Tuning the random forest hyperparameters
library(parallel)
library(parallelMap)
forestParamSpace<-makeParamSet(
  makeIntegerParam("ntree", lower=300, upper=300),
  makeIntegerParam("mtry", lower=6, upper=12),
  makeIntegerParam("nodesize", lower=1, upper = 5),
  makeIntegerParam("maxnodes", lower=5, upper=20)
)
randSearch<-makeTuneControlRandom(maxit=100)
cvForTuning<-makeResampleDesc("CV", iters=5)
parallelStartSocket(cpus=detectCores())
tunedForestPars<-tuneParams(forest, task=zooTask,
                            resampling = cvForTuning,
                            par.set=forestParamSpace,
                            control=randSearch)
parallelStop()
tunedForestPars
tunedForest<-setHyperPars(forest, par.vals=tunedForestPars$x)
tunedForestModel<-train(tunedForest, zooTask)

#Plotting the out-of-bag error
forestModelData<-getLearnerModel(tunedForestModel)
plot(forestModelData)
species<-colnames(forestModelData$err.rate)
legend("topright", species,
       col=1:length(species),
       lty=1:length(species))

#Corss-validatin the model building process
outer<-makeResampleDesc("CV", iters=5)
forestWrapper<-makeTuneWrapper("classif.randomForest", resampling = cvForTuning,
                               par.set = forestParamSpace,
                               control=randSearch)
parallelStartSocket(cpus=detectCores())
cvWithTuning<-resample(forestWrapper, zooTask, resampling = outer)
parallelStop()
cvWithTuning


#Building first XGBoost model
xgb<-makeLearner("classif.xgboost")
#Converting factors into numerics
zooXgb<-mutate_at(zooTib, .vars=vars(-type), .funs=as.numeric)
xgbTask<-makeClassifTask(data=zooXgb, target="type")                  
#Tuning XGBoost hyperparameters
xgbParamSpace<-makeParamSet(
  makeNumericParam("eta", lower=0, upper=1),
  makeNumericParam("gamma", lower=0, upper=5),
  makeIntegerParam("max_depth", lower=1, upper=5),
  makeNumericParam("min_child_weight", lower=1, upper=10),
  makeNumericParam("subsample", lower=0.5, upper=1),
  makeNumericParam("colsample_bytree", lower=0.5, upper=1),
  makeIntegerParam("nrounds", lower=20, upper=20),
  makeDiscreteParam("eval_metric", values=c("merror", "mlogloss"))
)
randSearch<-makeTuneControlRandom(maxit=1000)
cvForTuning<-makeResampleDesc("CV", iters=5)
tunedXgbPars<-tuneParams(xgb, task=xgbTask,
                         resampling = cvForTuning,
                         par.set = xgbParamSpace,
                         control=randSearch)
tunedXgbPars

#Training the final tuned model
tunedXgb<-setHyperPars(xgb, par.vals = tunedXgbPars$x)
tunedXgbModel<-train(tunedXgb, xgbTask)

#Plotting iteration number against log loss
xgbModelData<-getLearnerModel(tunedXgbModel)
ggplot(xgbModelData$evaluation_log, aes(iter, train_merror))+geom_line()+geom_point()

#Plotting individual decision trees
xgboost::xgb.plot.tree(model=xgbModelData, tress=1:5)

#Plotting individual decision trees
outer<-makeResampleDesc("CV", iters=3)
xgbWrapper<-makeTuneWrapper("classif.xgboost",
                            resampling = cvForTuning,
                            par.set = xgbParamSpace,
                            control=randSearch)
cvWithTuning<-resample(xgbWrapper, xgbTask, resampling = outer)
cvWithTuning



#Benchmarking algorithms against each other
#Plotting individual decision trees
learners=list(makeLearner("classif.knn"),
              makeLearner("classif.svm"),
              tunedForest,
              tunedXgb)
benchCV<-makeResampleDesc("RepCV", folds=10, reps=5)
bench<-benchmark(learners, xgbTask, benchCV)
bench
