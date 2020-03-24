#Import dataset
df<-read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", header = T)
#Overview of datasets
head(df)
str(df)
length(is.na(df))
dim(df)
summary(df)
#Handling missing values
library(tidyr)
df<-df%>%drop_na()
#Look at the number of unique values each column has
apply(df,2,function(x) length(unique(x)))
#Take a look at the distributions of some of these categori variables
ggplot(df%>%group_by(gender)%>%summarise(Count=n()),
       aes(x=gender, y=Count))+
  geom_bar(width=0.5,stat="identity")+
  ggtitle("")+
  xlab("Gender")+
  ylab("Count")+
  theme(plot.title = element_text(hjust=0.5))

ggplot(df%>%group_by(InternetService)%>%summarise(Count=n()),
       aes(x=InternetService, y=Count))+
  geom_bar(width=0.5, stat="identity")+
  ggtitle("")+
  xlab("Internet Service")+
  ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df%>%group_by(PaymentMethod)%>%summarise(Count=n()),
       aes(x=PaymentMethod, y=Count))+
  geom_bar(width=0.5, stat="identity")+
  ggtitle("")+
  xlab("Internet Service")+
  ylab("Count")+
  theme(plot.title = element_text(hjust=0.5))

#Binary&Continuous Vars
sampleDF<-df%>%
  select(tenure, MonthlyCharges, TotalCharges, gender, Partner, Dependents, PhoneService, PaperlessBilling, Churn)%>%
  mutate(
    #Transforming continuous vars
    tenure=(tenure-mean(tenure))/sd(tenure),
    MonthlyCharges=(log(MonthlyCharges)-mean(log(MonthlyCharges)))/sd(log(MonthlyCharges)),
    TotalCharges=(log(TotalCharges)-mean(log(TotalCharges)))/sd(log(TotalCharges)),
    #Encoding binary categorical vars
    gender=gender%>%as.factor()%>%as.numeric()-1,
    Partner=Partner%>%as.factor()%>%as.numeric()-1,
    Dependents=Dependents%>%as.factor()%>%as.numeric()-1,
    PhoneService=PhoneService%>%as.factor()%>%as.numeric()-1,
    PaperlessBilling=PaperlessBilling%>%as.factor()%>%as.numeric()-1,
    Churn=Churn%>%as.factor()%>%as.numeric()-1
  )
#See the distributions
summary(df[,c("tenure","MonthlyCharges","TotalCharges")])
apply(df[,c("tenure","MonthlyCharges","TotalCharges")],2,sd)
#Create Dummy vars
library(dummies)
sampleDF<-cbind(sampleDF, dummy(df$MultipleLines, sep="."))
names(sampleDF)=gsub("sampleDF","MultipleLines",names(sampleDF))

sampleDF<-cbind(sampleDF, dummy(df$InternetService, sep="."))
names(sampleDF)=gsub("sampleDF","InternetService", names(sampleDF))

sampleDF<-cbind(sampleDF, dummy(df$OnlineSecurity, sep="."))
names(sampleDF)=gsub("sampleDF","OnlineSecurity",names(sampleDF))

sampleDF<-cbind(sampleDF, dummy(df$OnlineBackup, sep="."))
names(sampleDF)=gsub("sampleDF","OnlineBackup",names(sampleDF))

sampleDF<-cbind(sampleDF, dummy(df$DeviceProtection, sep="."))
names(sampleDF)=gsub("sampleDF","DeviceProtection", names(sampleDF))

sampleDF<-cbind(sampleDF, dummy(df$TechSupport, sep="."))
names(sampleDF)=gsub("sampleDF", "TechSupport",names(sampleDF))

sampleDF<-cbind(sampleDF, dummy(df$StreamingTV, sep="."))
names(sampleDF)=gsub("sampleDF", "StreamingTV", names(sampleDF))

sampleDF<-cbind(sampleDF, dummy(df$StreamingMovies, sep="."))
names(sampleDF)=gsub("sampleDF", "StreamingMovies", names(sampleDF))

sampleDF<-cbind(sampleDF, dummy(df$Contract, sep="."))
names(sampleDF)=gsub("sampleDF", "Contract",names(sampleDF))

sampleDF<-cbind(sampleDF, dummy(df$PaymentMethod, sep="."))
names(sampleDF)=gsub("sampleDF", "PaymentMethod", names(sampleDF))

#ANN with Keras
library(keras)
model<-keras_model_sequential()
