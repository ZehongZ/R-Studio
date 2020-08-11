#Install packages
library(mlr)
library(tidyverse)

#Loading and exploring the Iowa dataset
data(Iowa, package="lasso2")
iowaTib<-as_tibble(Iowa)
iowaTib

#Plotting the data
iowaUntidy<-gather(iowaTib, "Variable","Value",-Yield)
ggplot(iowaUntidy, aes(Value, Yield))+
  facet_wrap(~Variable, scales="free_x")+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()

#Creating the task and learner
library(glmnet)
iowaTask<-makeRegrTask(data = iowaTib, target="Yield")
ridge<-makeLearner("regr.glmnet", alpha=0, id="ridge")

#Generating and plotting filter values
library(randomForestSRC)
filterVals<-generateFilterValuesData(iowaTask)
plotFilterValues(filterVals)+theme_bw()

#Tuning the lambda(s) hyperparameter
ridgeParamSpace<-makeParamSet(
  makeNumericParam("s",lower = 0, upper=15)
)
randSearch<-makeTuneControlRandom(maxit=200)
cvForTuning<-makeResampleDesc("RepCV", folds=3, reps=10)
library(parallel)
library(parallelMap)
parallelStartSocket(cpus=detectCores())
tunedRidgePars<-tuneParams(ridge, task=iowaTask,
                           resampling = cvForTuning,
                           par.set=ridgeParamSpace,
                           control=randSearch)
parallelStop()
tunedRidgePars

#Plotting the hyperparameter tuning process
ridgeTuningData<-generateHyperParsEffectData(tunedRidgePars)
plotHyperParsEffect(ridgeTuningData, x="s",y="mse.test.mean",plot.type="line")+theme_bw()

#Training a ridge regression model using the tuned lambda
tunedRidge<-setHyperPars(ridge, par.vals=tunedRidgePars$x)
tunedRidgeModel<-train(tunedRidge, iowaTask)

#Extracting the model parameters
ridgeModelData<-getLearnerModel(tunedRidgeModel)
ridgeCoefs<-coef(ridgeModelData, s=tunedRidgePars$x$s)
ridgeCoefs

#Plotting the mdoel parameters
lmCoefs<-coef(lm(Yield~., data=iowaTib))
coefTib<-tibble(Coef=rownames(ridgeCoefs)[-1],
                Ridge=as.vector(ridgeCoefs)[-1],
                Lm=as.vector(lmCoefs)[-1])
coefUntidy<-gather(coefTib, key=Model, value=Beta, -Coef)
ggplot(coefUntidy, aes(reorder(Coef, Beta), Beta, fill=Model))+
  geom_bar(stat="identity", col="black")+
  facet_wrap(~Model)+
  theme_bw()+
  theme(legend.position = "none")

#Training the LASSO model
lasso<-makeLearner("regr.glmnet", alpha=1, id="lasso")

#Tuning lambda for LASSO
lassoParamSpace<-makeParamSet(
  makeNumericParam("s", lower=0, upper=15)
)
parallelStartSocket(cpus=detectCores())
tunedLassoPars<-tuneParams(lasso, task = iowaTask,
                           resampling = cvForTuning,
                           par.set=lassoParamSpace,
                           control=randSearch)
parallelStop()
tunedLassoPars

#Plotting the hyperparameter tuning process
lassoTuningData<-generateHyperParsEffectData(tunedLassoPars)
plotHyperParsEffect(lassoTuningData,x="s",y="mse.test.mean",
                    plot.type = "line")+theme_bw()

#Training a LASSO model using the tuned lambda
tunedLasso<-setHyperPars(lasso, par.vals=tunedLassoPars$x)
tunedLassoModel<-train(tunedLasso, iowaTask)

#Extracting the model parameters
lassoModelData<-getLearnerModel(tunedLassoModel)
lassoCoefs<-coef(lassoModelData, s=tunedLassoPars$x$s)
lassoCoefs

#Plotting the model parameters
coefTib$LASSO<-as.vector(lassoCoefs)[-1]
coefUntidy<-gather(coefTib, key=Model, value=Beta, -Coef)
ggplot(coefUntidy, aes(reorder(Coef, Beta), Beta, fill=Model))+
  geom_bar(stat = "identity", col="black")+
  facet_wrap(~Model)+
  theme_bw()+
  theme(legend.position = "none")

#Training the elastic net model
elastic<-makeLearner("regr.glmnet", id="elastic")

#Tuning lambda and alpha for elastic net
elasticParamSpace<-makeParamSet(
  makeNumericParam("s", lower=0, upper=10),
  makeNumericParam("alpha", lower=0, upper = 1)
)
randSearchElastic<-makeTuneControlRandom(maxit=400)
parallelStartSocket(cpus = detectCores())
tunedElasticPars<-tuneParams(elastic, task=iowaTask,
                             resampling = cvForTuning,
                             par.set = elasticParamSpace,
                             control=randSearchElastic)
parallelStop()
tunedElasticPars

#Plotting the tuning process
library(kknn)
elasticTuningData<-generateHyperParsEffectData(tunedElasticPars)
plotHyperParsEffect(elasticTuningData, x="s", y="alpha",
                    z="mse.test.mean", interpolate = "regr.kknn",
                    plot.type = "heatmap")+
  scale_fill_gradientn(colours =terrain.colors(5))+
  geom_point(x=tunedElasticPars$x$s, y=tunedElasticPars$x$alpha,
             col="white")+theme_bw()

#Training an elastic net model using the tuned hyperparameters
tunedElastic<-setHyperPars(elastic, par.vals = tunedElasticPars$x)
tunedElasticModel<-train(tunedElastic, iowaTask)

#Plotting the model parameters
elasticModelData<-getLearnerModel(tunedElasticModel)
elasticCoefs<-coef(elasticModelData, s=tunedElasticPars$x$s)
coefTib$Elastic<-as.vector(elasticCoefs)[-1]
coefUntidy<-gather(coefTib, key=Model, value = Beta, -Coef)
ggplot(coefUntidy, aes(reorder(Coef, Beta),Beta, fill=Model))+
  geom_bar(stat="identity",position="dodge",col="black")+
  facet_wrap(~Model)+
  theme_bw()

#Plotting the model parameters
ridgeWrapper<-makeTuneWrapper(ridge, resampling=cvForTuning,
                              par.set = ridgeParamSpace,
                              control=randSearch)
lassoWrapper<-makeTuneWrapper(lasso, resampling=cvForTuning,
                              par.set = lassoParamSpace,
                              control=randSearch)
elasticWrapper<-makeTuneWrapper(elastic, resampling=cvForTuning,
                                par.set=elasticParamSpace,
                                control=randSearchElastic)
learners=list(ridgeWrapper, lassoWrapper, elasticWrapper, "regr.lm")

#Plotting the model parameters
library(parallel)
library(parallelMap)
kFold3<-makeResampleDesc("CV", iters=3)
parallelStartSocket(cpus=detectCores())
bench<-benchmark(learners, iowaTask, kFold3)
parallelStop()
bench

#