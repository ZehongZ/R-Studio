#Import dataset
credit<-read.csv("credit.csv", header=T)

#Overview of the dataset
str(credit)
head(credit)

table(credit$checking_balance)
prop.table(table(credit$checking_balance))

table(credit$savings_balance)
prop.table(table(credit$savings_balance))

summary(credit$months_loan_duration)
summary(credit$amount)

table(credit$default)#1 as no, 2 as yes

#Create random training and test datasets
#seed: causes the randomization process to follow a sequence that can be replicated later on if desired.
set.seed(123)
train_sample<-sample(1000,900)
str(train_sample)
credit_train<-credit[train_sample,]
credit_test<-credit[-train_sample,]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

#Training a model on the data
library(C50)
credit_model<-C5.0(credit_train[-17],as.factor(credit_train$default),trials = 1, costs = NULL)
credit_model
summary(credit_model)
plot(credit_model)

#Evaluating model performance
credit_pred<-predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("actual default", "predicted default"))

#Using adaptive boosting
#Simply increases trails numbers
credit_boost10<-C5.0(credit_train[-17], as.factor(credit_train$default), trials = 10)
credit_boost10
summary(credit_boost10)

#Evaluating model performance
credit_boost_pred10<-predict(credit_boost10,credit_test)
CrossTable(credit_test$default, credit_boost_pred10,prop.chisq = FALSE, prop.c = FALSE, prop.r=FALSE, dnn=c("actual default","predicted default"))

#Making mistakes more costlier than others
#Assign a penalty to different types of errors in order to discourage a tree from making more costly mistakes.
#Since the predicted and actual values can both take two values, yes or no, we need to describe a 2*2 matrix usng a list of two vectors
matrix_dimensions<-list(c("1","2"),c("1","2"))
matrix_dimensions
names(matrix_dimensions)<-c("predicted","actual")
matrix_dimensions

#Assign the penalty for the various types of errors 
error_cost<-matrix(c(0,1,4,0),nrow=2, dimnames = matrix_dimensions)
error_cost

#Build the model
credit_cost<-C5.0(credit_train[-17],as.factor(credit_train$default), costs=error_cost)
credit_cost_pred<-predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, 
           prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE,
           dnn=c("actual default", "predicted default"))

#Import dataset
mushrooms<-read.csv("mushrooms.csv", stringsAsFactors = TRUE)

#Overview of dataset
str(mushrooms)

#Drop the variable that is meaningless
mushrooms$veil_type<-NULL

#Check distribution of the mushroom type class
table(mushrooms$type)
prop.table(table(mushrooms$type))

#Training a model on the data
library(RWeka)
