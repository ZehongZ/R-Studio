#Import dataset
launch<-read.csv("challenger.csv",header=T)

#Covariance
b<-cov(launch$temperature,launch$distress_ct)/var(launch$temperature)
b
a<-mean(launch$distress_ct)-b*mean(launch$temperature)
a

#Correlation
r<-cov(launch$temperature, launch$distress_ct)/(sd(launch$temperature)*sd(launch$distress_ct))
r

#Matrix Notation of Multiple Linear
reg<-function(y,x){
  x<-as.matrix(x)
  x<-cbind(Intercept=1,x)
  b<-solve(t(x)%*%x)%*%t(x)%*%y#solve() takes inverse of the a matrix
  colnames(b)<-"estimate"
  print(b)
}
str(launch)
reg(y=launch$distress_ct, x=launch[2:4])

#Predicting Medical Expenses Using Linear Regression
#Import dataset
insurance<-read.csv("insurance.csv",stringsAsFactors = TRUE)

#Overview of the dataste
str(insurance)
summary(insurance$charges)#Because mean value is greater than median, hence it implies that the distribution if right-skewed

#Histogram
hist(insurance$charges)

#Distribution of region
table(insurance$region)
prop.table(table(insurance$region))

#Correlation Matrix
cor(insurance[c("age","bmi","children","charges")])

#Scatterplot Matrix-Visualize relationships among features
pairs(insurance[c("age","bmi","children","charges")])

#Scatterplot with panel
library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])

#Training a model on the data
ins_model<-lm(charges~age+children+bmi+sex+smoker+region, data=insurance)
ins_model

#Evaluating model performance
summary(ins_model)

#Transformation-convertinga numeric variable to a binary indicator
insurance$bmi30<-ifelse(insurance$bmi>=30,1,0)

#Transform variable to a higher order
insurance$age2<-insurance$age^2

#An improved regression model
ins_model2<-lm(charges~age+age2+children+bmi+sex+bmi30*smoker+region, data=insurance)
summary(ins_model2)

#Estimating the quality of wines with regression tress and model trees
#Import dataset
wine<-read.csv("whitewines.csv", header = T)

#Overview of the dataset
str(wine)

#Distribution of target
hist(wine$quality)

#Divide into training and testing sets
wine_train<-wine[1:3750,]
wine_test<-wine[3751:4898,]

#Training a model on the data
library(rpart)
#model tree
m.rpart<-rpart(quality~., data=wine_train)
m.rpart

#Visualize decision tree
library(rpart.plot)
rpart.plot(m.rpart, digits=3)#Digits control the number of numeric digits to include in the diagram
rpart.plot(m.rpart, digits=4, fallen.leaves = TRUE, type=3, extra=101)
#Fallen leaves parameters forces the leaf nodes to be aligned at the bottom of the plot
#type and extra parameters affect the way the decisions and nodes are labeled

#Evaluating model performance
p.rpart<-predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)

#Using correlatin to measure performance
cor(p.rpart, wine_test$quality)

#Mean absolute error
MAE<-function(actual, predicted){
  mean(abs(actual-predicted))
}
MAE(p.rpart,wine_test$quality)

#Improve model performance (Building model tree)
library(RWeka)
