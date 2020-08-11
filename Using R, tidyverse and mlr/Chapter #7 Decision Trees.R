#Building our first decision tree model
library(mlr)
library(tidyverse)

#Loading and exploring the zoo dataset
data(Zoo, package = "mlbench")
zooTib<-as_tibble(Zoo)
zooTib

#Converting logical variables to factors
zooTib<-mutate_if(zooTib, is.logical, as.factor)

#Creating the task and learner
zooTask<-makeClassifTask(data=zooTib, target = "type")
tree<-makeLearner("classif.rpart")

#Printing available rpart hyperparameters
getParamSet(tree)

#Defining the hyperparameter space for tuning
treeParamSpace<-makeParamSet(
  makeIntegerParam("minsplit", lower=5, upper=20),
  makeIntegerParam("minbucket", lower=3, upper=10),
  makeNumericParam("cp",lower=0.01, upper=0.1),
  makeIntegerParam("maxdepth", lower=3, upper=10)
)

#Defining the random search
randSearch<-makeTuneControlRandom(maxit=200)
cvForTuning<-makeResampleDesc("CV", iters=5)

#Performing hyperparameter tuning
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())
tunedTreePars<-tuneParams(tree, task=zooTask,
                          resampling=cvForTuning,
                          par.set = treeParamSpace,
                          control=randSearch)

#Defining the random search
randSearch<-makeTuneControlRandom(maxit=200)
cvForTuning<-makeResampleDesc("CV", iters=5)

#Performing hyperparameter tuning
library(parallel)
library(parallelMap)
parallelStartSocket(cpus=detectCores())
tunedTreePars<-tuneParams(tree, task=zooTask,
                          resampling = cvForTuning,
                          par.set=treeParamSpace,
                          control=randSearch)
parallelStop()
tunedTreePars

#Training the final tuned model
tunedTree<-setHyperPars(tree, par.vals = tunedTreePars$x)
tunedTreeModel<-train(tunedTree, zooTask)

#Plotting the decision tree
library(rpart.plot)
treeModelData<-getLearnerModel(tunedTreeModel)
rpart.plot(treeModelData, roundint=FALSE,
           box.palette = "BuBn",
           type=5)

#Exploring the model
printcp(treeModelData, digits=3)

#Cross-validating the model-building process
outer<-makeResampleDesc("CV", iters=5)
treeWrapper<-makeTuneWrapper("classif.rpart", resampling= cvForTuning,
                             par.set=treeParamSpace,
                             control=randSearch)

#Extracting the cross-validation results
ewithRunings
