####Understanding ensembles####
#Based on the idea that by combining multiple weaker learners, a stronger learner is created

####Bagging####
#Bootstrap aggregating or bagging
#These datasets are then used to generate a set of models using a single learning algorithm
#The models' predictions are combined using voting (for classification) or averaging (for numeric prediction)
library(ipred)
set.seed(300)
myd=read.csv("credit.csv",header = T)
mybag<-bagging(default~., data=myd, nbagg=25)
credit_pred<-predict(mybag,myd)
table(credit_pred, myd$default)

library(caret)
set.seed(300)
ctrl<-trainControl(method="cv",number=10)
train(default~., data=myd, method="treebag",trControl=ctrl)

####Boosting####
#It boosts the performance of weak learners to attain the performance of stronger learners
#Boosting are constructed specifically to generate complementary learners
#Boosting gives each learner's vote a weight based on its past performance
#A boosting algorithm called AdaBoost. The algorithm is based on the idea of generating weak learners that iteratively learn a larger portion of the difficult-to-classify examples by paying more attention to frequently misclassified examples
library(C50)
library(adabag)
set.seed(300)
m_adaboost<-boosting(default~., data=credit)
p_adaboost<-predict(m_adaboost, myd)
