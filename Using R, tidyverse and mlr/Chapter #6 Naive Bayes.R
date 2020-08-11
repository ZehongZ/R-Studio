#Building first naive Bayes model
library(mlr)
library(tidyverse)

#Loading the exploring the HouseVotes84 dataset
data(HouseVotes84, package="mlbench")
votesTib<-as_tibble(HouseVotes84)
votesTib
names(votesTib)

#Showing missing values
map_dbl(votesTib, ~sum(is.na(.)))

#Plotting the HouseVotes84 dataset
votesUntidy<-gather(votesTib, "Variable", "Value",-Class)
votesUntidy
ggplot(votesUntidy, aes(Class, fill=Value))+
  facet_wrap(~Variable, scales="free_y")+
  geom_bar(position="fill")+
  theme_bw()

#Creating the task and learner, and training the model
votesTask<-makeClassifTask(data=votesTib, target="Class")
bayes<-makeLearner("classif.naiveBayes")
bayesModel<-train(bayes, votesTask)

#Cross-validating the naive Bayes model
kFold<-makeResampleDesc(method="RepCV", folds=10, reps=50,
                        stratify=TRUE)
bayesCV<-resample(learner = bayes, task=votesTask,
                  resampling = kFold,
                  measures=list(mmce, acc, fpr,fnr))
bayesCV$aggr

#Using the model to make predictions
politician<-tibble(V1="n", V2="n", V3="y", V4="n",V5="n",
                   V6="y", V7="y", V8="y", V9="y", V10="y",
                   V15="y", V16="n")
politicianPred<-predict(bayesModel, newdata = politician)
getPredictionResponse(politicianPred)

