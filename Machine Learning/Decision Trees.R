####Basic Concepts and Terms####
#Decision tree is a type of supervised learning algorithm that can be used in both regression and classification problems. 
#It works for both categorical and continuous input and output variables
#Root Node: Entire population or sample
#Decision Node: When a sub-node splits into further sub-nodes, it is called a Decision Node
#Terminal Node/Leaf: Nodes that do not split
#Pruning: remove sub-nodes of a decision node

####Regression Tree####
#In order to build a regression tree, you first use recursive binary splitting to grow a large tree on the training data, stopping only when each terminal node has fewer than some minimum number of observations
#Recursive Binary Splitting is a greedy and top-down algorithm using minimum the Residual Sum of Squares.
#Beginning at the top of the tree, split it into 2 branches, creating a partition of 2 spaces;
#Then carry out this particular split at the top of the tree multiple times and choose the split of the features that minimizes the (current)RSS
#Your goal is to select the one that leads to the lowest error rate

####Classification Tree####
#A classification tree is very similar to a regression tree, except that it is used to predict a qualitative response rather than a quantitative one
#In the classification settting, Residual Sum of Squares cannot be used as a criterion for making the binary splits.
#Classification Error Rate: You can define the "hit rate" as the fraction of training observations in a particular region that don't belong to the most widely occuring class
#Gini Index:The Gini Index is an alternative error metric that is designed to show how "pure" a region is. 
#"Purity" in this case means how much of training data in a paritcular region belongs to a single class. If a region contains data that is mostly from a single class then the Gini Index value will be small

####Cross-Entropy/Deviance####
#It is quiet similar numerically to Gini Index

#Either the Gini index or the cross-entropy are typically used to evaluate the quality of a particular split since they are more sensitive to node purity than is the classification error rate

####Bagging####
#The decision trees suffer from high variance. 
#If you split the training data into 2 parts at random, and fit a decision tree to both halves, the results that you get could be quiet different
#Bagging/Bootstrap is a technique used to reduce the variance of your predictions by combining the result of multiple classifiers modeled on differnt sub-samples of the same dataset

####Random Forest####
#Random Forests is a versatile machine learning method capable of performing both regression and classification tasks.
#It also undertakes dimensional reduction methods
#Random Forests decorrelates the treees.

####Boosting####
#Boosting grows the tree sequentially. 
#Each tree is grown using information from previously grown trees
#Boosting does not involve bootstrap sampling; each tree is fitted on a modified version of the original dataset

####Decision Trees in R####
library(rpart)
myd=read.csv("winedata.csv",header = T)
train.size=round(0.66*nrow(myd))
id.train=sample(1:nrow(myd),train.size, replace=FALSE)
myd.train=myd[id.train,]
myd.test=myd[-id.train,]
dtree=rpart(grape~., data=myd.train,method="class", parms = list(split="gini"))
train.pred=predict(dtree, newdata=myd.train, type="class")
train.perf=table(myd.train$grape, train.pred, dnn=c("Actual","Predicted"))
prop.table(train.perf,1)
accuracy=sum(diag(train.perf))/sum(train.perf)
accuracy

####Random Forest####
library(randomForest)
library(caret)
myd=read.csv("winedata.csv",header = T)
forest=randomForest(as.factor(grape)~., data=myd, importance=TRUE,ntree=100)
raw.pred=predict(forest,newdata = myd)
raw.perf=table(myd$grape, raw.pred, dnn=c("Actual","Predicted"))
raw.perf

####Boosting####
library(gbm)
myd=read.csv("winedata.csv",header = T)
train.size=round(0.66*nrow(myd))
id.train=sample(1:nrow(myd),train.size, replace=FALSE)
myd.train=myd[id.train,]
myd.test=myd[-id.train,]
boost.myd=gbm(grape~., data=myd.train, distribution = "gaussian",n.trees = 10000, shrinkage = 0.01,interaction.depth = 4)
summary(boost.myd)
plot(boost.myd,i="flav")
#Predict on test set
n.trees=seq(from=100, to=10000, by=100)
predmat=predict(boost.myd,newdata=myd.test,n.trees=n.trees)
dim(predmat)

####Choosing the best split####
#Purity: a subset of examples contains only a single class
#Pure: subset composed of only a single class
#Entropy:Sets with high entropy are very diverse and provide little information about other items that may also belong in the set, as there is no apparent commonality
#The decision tree hopes to find splits that reduce entropy, ultimately increasing homogeneity within the groups
#Information Gain: the algorithm calculates the change in homogeneity that would result from a split on each possible feature
#The higher the information gain, the better a feature is at creating homogeneous groups after a split on this feature
#If the information gain is zero, there is no reduction in entropy for splitting on this feature
#The maximum information gain is equal to the entropy prior to the split. This would imply that the entropy after the split is zero, which means that the split results in completely homogeneous groups

####Identifying risky bank loans####
credit<-read.csv("german_credit (1).csv",header = T)
str(credit)
names(credit)
head(credit$Creditability)
levels(as.factor(credit$Creditability))
table(credit$Account.Balance)
table(credit$Value.Savings.Stocks)
summary(credit$Duration)
#Data preparation
set.seed(123)
credit$Creditability=as.factor(credit$Creditability)
train_sample<-sample(1000,900)
credit_train<-credit[train_sample,]
credit_test<-credit[-train_sample,]
prop.table(table(credit_train$Creditability))
prop.table(table(credit_test$Creditability))
#Model
library(C50)
credit_model<-C5.0(credit_train[-1],credit_train$Creditability)
credit_model
summary(credit_model)
#Evaluating model performance
library(gmodels)
credit_pred<-predict(credit_model,credit_test)
CrossTable(credit_test$Creditability,credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn=c("actual credibility","predicted credibility"))
#Improving model performance
credit_boost10<-C5.0(credit_train[-1],credit_train$Creditability,trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost10_pred<-predict(credit_boost10,credit_test)
CrossTable(credit_test$Creditability,credit_boost10_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn=c("Actual","Predicted"))
#Making mistakes more costlier than others
matrix_dimensions<-list(c("no","yes"),c("no","yes"))
names(matrix_dimensions)<-c("predicted","actual")
matrix_dimensions
error_cost<-matrix(c(0,1,4,0),nrow = 2,dimnames = matrix_dimensions)
error_cost
credit_cost<-C5.0(credit_train[-1],credit_train$Creditability,costs=error_cost)
credit_cost_pred<-predict(credit_cost,credit_test)

