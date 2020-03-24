#Load libraries
library(dplyr)
library(ggplot2)
#Load dataset
df<-read.csv("WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv", header = T, sep = ",")
#Subset
subdf<-df[,c("Customer","State","Customer.Lifetime.Value","Response","Coverage","Education","Effective.To.Date","EmploymentStatus","Gender","Income","Location.Code")]
#Overview of dataset
head(df)
head(subdf)
#Encode Response as 0s and 1s
df$Engaged<-as.integer(df$Response)-1
head(df$Engaged)
#Calculate Engagement Rate
engagementRate<-df%>%
  group_by(Engaged)%>%
  summarise(Count=n())%>%
  mutate(Percentage=Count/nrow(df)*100.0)
engagementRate
#Transpose for easy to read
transposed<-t(engagementRate)
colnames(transposed)<-engagementRate$Engaged
transposed<-transposed[-1,]
transposed
#Sales Channels
salesChannel<-df%>%
  group_by(Engaged, Channel=Sales.Channel)%>%
  summarise(Count=n())
salesChannel
#Pie char to differntiate customers
ggplot(salesChannel, aes(x="",y=Count, fill=Channel))+
  geom_bar(width=1, stat="identity",position=position_fill())+
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust=0.5))+
  coord_polar("y")+
  facet_wrap(~Engaged)+
  ggtitle('Sales Channel(0:Not Engagaed, 1:Engaged)')+
  theme(
    axis.title.x = element_blank(),
    axis.title.y=element_blank(),
    plot.title = element_text(hjust=0.5),
    legend.position = 'bottom'
  )
ggplot(df, aes(x="",Total.Claim.Amount))+
  geom_boxplot()+
  facet_wrap(~Engaged)+
  ylab("Total Claim Amount")+
  xlab("0:Not Engaged, 1:Engaged")+
  ggtitle("Engaged vs. Not Engaged: Total Claim Amount")+
  theme(plot.title = element_text(hjust=0.5))
#Without outliers
ggplot(df, aes(x="",y=Total.Claim.Amount))+
  geom_boxplot(outlier.shape=NA)+
  scale_y_continuous(limits = quantile(df$Total.Claim.Amount,c(0.1,0.9)))+
  facet_wrap(~Engaged)+
  ylab("Total Claim Amount")+
  xlab("0:Not Engaged, 1:Engaged")+
  ggtitle("Engaged vs. Not Engaged: Total Claim Amount")+
  theme(plot.title = element_text(hjust=0.5))
#Get data types of each column
sapply(df,class)
summary(df)
#Get numeric columns
continuousDF<-select_if(df, is.numeric)
colnames(continuousDF)
#Fit regression model with continous variable
logit_fit<-glm(Engaged~., data=continuousDF, family = binomial)
summary(logit_fit)
#Fit regression model with Educatin factor variables
logit.fit<-glm(Engaged~factor(Education), data=df, family = binomial)
summary(logit.fit)
#Fit regression model with Education & Gender variables
logit.fit<-glm(Engaged~factor(Education)+factor(Gender), data=df, family = binomial)
summary(logit.fit)
#Combining continuous and categorical variables
continuousDF$Gender<-factor(df$Gender)
continuousDF$Education<-factor(df$Education)
colnames(continuousDF)
#Fit regression model with Education & Gender variables
logit.fit<-glm(Engaged~., data=continuousDF, family=binomial)
summary(logit.fit)
