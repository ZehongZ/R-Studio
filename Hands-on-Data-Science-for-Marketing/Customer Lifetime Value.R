#Load libraries
library(dplyr)
library(readxl)
#Load data
df<-read_excel("Online Retail.xlsx", sheet="Online Retail")
#Handling negative quantity
df<-df[which(df$Quantity>0),]
#Drop NA records
df<-na.omit(df)
#Handling incomplete data
sprintf("Data Range: %s-%s", min(df$InvoiceDate), max(df$InvoiceDate))
df<-df[which(df$InvoiceDate<"2011-12-01"),]
#Create a column for total sales
df$Sales<-df$Quantity*df$UnitPrice
#Summarise data for each order 
ordersDF<-df%>%
  group_by(CustomerID, InvoiceNo)%>%
  summarise(Sales=sum(Sales),InvoiceDate=max(InvoiceDate))
#Order amount&frequency summary
summaryDF<-ordersDF%>%
  group_by(CustomerID)%>%
  summarize(
    SalesMin=min(Sales), SalesMax=max(Sales), SalesSum=sum(Sales),
    SalesAvg=mean(Sales), SalesCount=n(),
    InvoiceDateMin=min(InvoiceDate), InVoiceDateMax=max(InvoiceDate),
    PurchaseDuration=as.double(floor(max(InvoiceDate)-min(InvoiceDate))),
    PurchaseFrequency=as.double(floor(max(InvoiceDate)-min(InvoiceDate)))/n()
  )
#Distributions of the number of purchases that the repeat customers have mode
summaryDF<-summaryDF[which(summaryDF$PurchaseDuration>0),]#Exclude customers with only one purchase.
salesCount<-summaryDF%>%
  group_by(SalesCount)%>%
  summarise(Count=n())
library(ggplot2)
ggplot(salesCount[1:19,], aes(x=SalesCount, y=Count))+
  geom_bar(width=0.5, stat="identity")+
  ggtitle("")+
  xlab("Sales Count")+
  ylab("Count")+
  theme(plot.title=element_text(hjust=0.5))
#Frequency between purchases for repeat customers
hist(
  summaryDF$PurchaseFrequency,
  breaks=20,
  xlab="Average Number of days between purchases",
  ylab="count",
  main = ""
)

###Predicting the 3 month CLV
#Group data into every 3 months
library(lubridate)
ordersDF$Quarter=as.character(round_date(ordersDF$InvoiceDate,'3 months'))
dataDF<-ordersDF%>%
  group_by(CustomerID, Quarter)%>%
  summarize(SalesSum=sum(Sales), SalesAvg=mean(Sales), SalesCount=n())
dataDF$Quarter[dataDF$Quarter=="2012-01-01"]<-"Q1"
dataDF$Quarter[dataDF$Quarter=="2011-10-01"]<-"Q2"
dataDF$Quarter[dataDF$Quarter=="2011-07-01"]<-"Q3"
dataDF$Quarter[dataDF$Quarter=="2011-04-01"]<-"Q4"
dataDF$Quarter[dataDF$Quarter=="2011-01-01"]<-"Q5"
#Reshape the dataset into a tabular data
library(reshape2)
salesSumFeatureDF<-dcast(
  dataDF[which(dataDF$Quarter!="Q1"),],
  CustomerID~Quarter,
  value.var="SalesSum"
)
colnames(salesSumFeatureDF)<-c("CustomerID","SalesSumQ2","SalesSumQ3","SalesSumQ4","SalesSumQ5")
salesAvgFeaturesDF<-dcast(
  dataDF[which(dataDF$Quarter!="Q1"),],
  CustomerID~Quarter,
  value.var="SalesAvg"
)
colnames(salesAvgFeaturesDF)<-c("CustomerID","SalesAvgQ2","SalesAvgQ3","SalesAvgQ4","SalesAvgQ5")
salesCountFeatureDF<-dcast(
  dataDF[which(dataDF$Quarter!="Q1"),],
  CustomerID~Quarter,
  value.var="SalesCount"
)
colnames(salesCountFeatureDF)<-c("CustomerID","SalesCountQ2","SalesCountQ3","SalesCountQ4","SalesCountQ5")
featuresDF<-merge(
  merge(salesSumFeatureDF, salesAvgFeaturesDF, by="CustomerID"),
  salesCountFeatureDF, by="CustomerID"
)
featuresDF[is.na(featuresDF)]<-0
#Build target varaiables
responseDF<-dataDF[which(dataDF$Quarter=="Q1"),]%>%
  select(CustomerID, SalesSum)
colnames(responseDF)<-c("CustomerID","CLV_3_Month")
#Build a sample set for machine learning model
sampleDF<-merge(featuresDF, responseDF, by="CustomerID", all.x = TRUE)
sampleDF[is.na(sampleDF)]<-0
###Linear regression
library(caTools)
sample<-sample.split(sampleDF$CustomerID, SplitRatio = .8)
train<-as.data.frame(subset(sampleDF, sample=TRUE))[,-1]
test<-as.data.frame(subset(sampleDF, sample=FALSE))[,-1]
regFit<-lm(CLV_3_Month~., data=train)
summary(regFit)
#Evaluating regression model performance
train_preds<-predict(regFit, train)
test_preds<-predict(regFit, test)
#Compute in-sample and out-of-sample R values
library(miscTools)
InSampleR2<-rSquared(train$CLV_3_Month, resid = train$CLV_3_Month-train_preds)
outSampleR2<-rSquared(test$CLV_3_Month, resid=test$CLV_3_Month-test_preds)
sprintf('In-Sample R-Squared: %0.4f', InSampleR2)
sprintf('Out-Sample R-Squared: %0.4f', outSampleR2)
#Median Absolute Error
inSampleMAE<-median(abs(train$CLV_3_Month-train_preds))
outSampleMAE<-median(abs(test$CLV_3_Month-test_preds))
sprintf('In-Sample MAE: %0.4f', inSampleMAE)
sprintf('Out-Sample MAE: %0.4f', outSampleMAE)
#Scatter plot of predicted vs actual
plot(
  test$CLV_3_Month,
  test_preds,
  xlab='actual',
  ylab='predicted',
  main='Out-of-Same Actual vs Predicted'
)
abline(a=0, b=1)
