#Load data
df<-read.csv("Marketing-Customer-Value-Analysis.csv", header=TRUE)

#Overview of data
summary(df)

#Response Variable
df$Engaged<-as.integer(df$Response)-1
mean(df$Engaged)

#Categorical Variable Encoding
categoricalVars=c(
  "Sales.Channel", "Vehicle.Size", "Vehicle.Class", "Policy", "Policy.Type",
  "EmploymentStatus", "Marital.Status","Education","Coverage","Gender"
)

#Transform categorical variables into dummy
encodedDF<-model.matrix(~.-1, df[categoricalVars])
summary(encodedDF)

#Continuous Features
continuousFeatures<-c(
  'Customer.Lifetime.Value', 'Income', 'Monthly.Premium.Auto',
  'Months.Since.Last.Claim', 'Months.Since.Policy.Inception',
  'Number.of.Open.Complaints', 'Number.of.Policies','Total.Claim.Amount'
)
encodedDF<-cbind(encodedDF, df[continuousFeatures])

#Building predictive models
library(caTools)
sample<-sample.split(df$Customer, SplitRatio = .7)
trainX<-as.matrix(subset(encodedDF, sample==TRUE))
trainY<-as.double(as.matrix(subset(df$Engaged, sample==TRUE)))
testX<-as.matrix(subset(encodedDF, sample==FALSE))
testY<-as.double(as.matrix(subset(df$Engaged, sample==FALSE)))
dim(encodedDF)
dim(trainX)
dim(testX)

#Random Forest Model
library(randomForest)
rfModel<-randomForest(x=trainX, y=factor(trainY), ntree=200, maxnodes=24)
getTree(rfModel,1)
predict(rfModel, trainX, predict.all = TRUE)
importance(rfModel)
#Make predictions
inSamplePreds<-as.double(predict(rfModel, trainX))-1
outSamplePreds<-as.double(predict(rfModel, testX))-1
#Accuracy
inSampleAccuracy<-mean(trainY==inSamplePreds)
outSampleAccuracy<-mean(testY==outSamplePreds)
print(sprintf('In-Sample Accuracy: %0.4f', inSampleAccuracy))
print(sprintf('Out-sample Accuracy: %0.4f ',outSampleAccuracy))
#Precision
inSamplePrecision<-sum(inSamplePreds&trainY)/sum(inSamplePreds)
outSamplePrecision<-sum(outSamplePreds&testY)/sum(outSamplePreds)
print(sprintf("In-Sample Precision: %0.4f",inSamplePrecision))
print(sprintf("Out-sample Precision: %0.4f", outSamplePrecision))
#Recall
inSampleRecall<-sum(inSamplePreds&trainY)/sum(trainY)
outSampleRecall<-sum(outSamplePreds&testY)/sum(testY)
print(sprintf("In-sample Recall: %0.4f", inSampleRecall))
print(sprintf("Out-sample Recall: %0.4f", outSampleRecall))
#ROC Curve
library(ROCR)
inSamplePredProbs<-as.double(predict(rfModel, trainX, type='prob')[,2])
outSamplePredProbs<-as.double(predict(rfModel, testX, type='prob')[,2])
pred<-prediction(outSamplePredProbs,testY)
perf<-performance(pred, measure="tpr", xmeasure="fpr")
auc<-performance(pred, measure='auc')@y.values[[1]]
plot(
  perf,
  main=sprintf('Random Forest Model ROC Curve (AUC:%0.2f)',auc),
  col='darkorange',
  lwd=2
)+grid()
abline(a=0, b=1, col='darkgray',lty=3, lwd=2)
