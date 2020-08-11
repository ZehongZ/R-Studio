#Building first SVM model
library(mlr)
library(tidyverse)

#Loading and exploring the spam dataset
data(spam, package="kernlab")
spamTib<-as_tibble(spam)
spamTib

#Tuning our parameters
spamTask<-makeClassifTask(data=spamTib, target="type")
svm<-makeLearner("classif.svm")

#Pring available SVM hyperparameters
getParamSet("classif.svm")

#Extracting the possible values for a hyperparameter
getParamSet("classif.svm")$pars$kernel$values

#Defining the hyperparameter space for tuning
kernels<-c("polynomial", "radial", "sigmoid")
svmParamSpace<-makeParamSet(
  makeDiscreteParam("kernel", values=kernels),
  makeIntegerParam("degree", lower=1, upper=3),
  makeNumericParam("cost", lower=0.1,upper=10),
  makeNumericParam("gamma", lower=0.1, 10)
)

#Defining the random search
randSearch<-makeTuneControlRandom(maxit=20)
cvForTuning<-makeResampleDesc("Holdout", split=2/3)

#Performing hyperparameter tuning
library(parallelMap)
library(parallel)
parallelStartSocket(cpus=detectCores())
tunedSvmPars<-tuneParams("classif.svm", task=spamTask,
                         resampling = cvForTuning,
                         par.set=svmParamSpace,
                         control=randSearch)

#Extracting the winnning hyperpqramwter values from tuning
tunedSvmPars
tunedSvmPars$x

#Training the model with the tuned hyperparameters
tunedSvm<-setHyperPars(makeLearner("classif.svm"),
                       par.vals = tunedSvmPars$x)
tunedSvmModel<-train(tunedSvm, spamTask)

#Cross-validating the model-building process
outer<-makeResampleDesc("CV",iters=3)
svmWrapper<-makeTuneWrapper("classif.svm",resampling = cvForTuning,
                            par.set = svmParamSpace,
                            control = randSearch)
parallelStartSocket(cpus=detectCores())
cvWithTuning<-resample(svmWrapper, spamTask, resampling = outer)
parallelStop()

#Extracting the cross-validation result
cvWithTuning
