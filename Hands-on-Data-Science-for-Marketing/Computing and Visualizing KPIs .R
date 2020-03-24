#Import dataset
conversionsDF<-read.csv("bank-additional-full.csv", header=TRUE, sep=";")
#Overview of dataset
str(conversionsDF)
head(conversionsDF)
#Encode conversions as 0s and 1s
conversionsDF$conversion<-as.integer(conversionsDF$y)-1
head(conversionsDF$conversion)
#Total number of conversions
sum(conversionsDF$conversion)
#Total number of clients in the data
nrow(conversionsDF)
#Aggregate Conversion Rate
sprintf("Conversion rate: %0.2f%%", sum(conversionsDF$conversion)/nrow(conversionsDF)*100.0)
#Conversion rates by age
library(dplyr)
conversionsByAge<-conversionsDF%>%
  group_by(Age=age)%>%
  summarise(TotalCount=n(), NumConversions=sum(conversion))%>%
  mutate(ConversionRate=NumConversions/TotalCount*100)
#Plot the conversion rate
library(ggplot2)
ggplot(data=conversionsByAge, aes(x=Age, y=ConversionRate))+geom_line()+ggtitle("Conversion Rates by Age")+xlab("Age")+ylab("Conversion Rate")+theme(plot.title=element_text(hjust=0.5))
#Group multiple age together
conversionsByAgeGroup<-conversionsDF%>%
  group_by(AgeGroup=cut(age,breaks = seq(20,70, by=10)))%>%
  summarise(TotalCount=n(), NumConversions=sum(conversion))%>%
  mutate(ConversionRate=NumConversions/TotalCount*100.0)
conversionsByAgeGroup$AgeGroup<-as.character(conversionsByAgeGroup$AgeGroup)
conversionsByAgeGroup$AgeGroup[6]<-"70+"
#Build bar chart
ggplot(conversionsByAgeGroup, aes(x=AgeGroup, y=ConversionRate))+
  geom_bar(width=0.5, stat="identity")+
  ggtitle("Conversion Rate by Age Groups")+
  xlab("Age")+
  ylab("Conversion Rate(%)")+
  theme(plot.title = element_text(hjust=0.5))

###Conversions vs Non-cnversions
#Count the number of conversions and non-conversions for each marital status
conversionsByMaritalStatus<-conversionsDF%>%
  group_by(Marital=marital, Conversion=conversion)%>%
  summarise(Count=n())
#Using Pie Chart
ggplot(conversionsByMaritalStatus, aes(x="",y=Count, fill=Marital))+
  geom_bar(width=1, stat="identity",position=position_fill())+
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust=0.5))+
  coord_polar("y")+facet_wrap(~Conversion)+
  ggtitle('Marital Status(0:Non Conversions,1:Conversions')+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position = 'bottom'
  )

###Conversions by age and marital status###
conversionsByAgeMarital<-conversionsDF%>%
  group_by(AgeGroup=cut(age, breaks = seq(20,70,by=10)),Marital=marital)%>%
  summarise(Count=n(), NumConversions=sum(conversion))%>%
  mutate(TotalCount=sum(Count))%>%
  mutate(ConversionRate=NumConversions/TotalCount)
conversionsByAgeMarital$AgeGroup<-as.character(conversionsByAgeMarital$AgeGroup)
conversionsByAgeMarital$AgeGroup[is.na(conversionsByAgeMarital$AgeGroup)]<-"70+"
#Bar chart
ggplot(conversionsByAgeMarital, aes(x=AgeGroup, y=ConversionRate, fill=Marital))+
  geom_bar(width=0.5, stat="identity", position="dodge")+
  ylab("Conversion Rate(%)")+
  xlab("Age")+
  ggtitle("Conversion Rates by Age and Marital Status")+
  theme(plot.title = element_text(hjust=0.5))
#Stacked Bar Chart
ggplot(conversionsByAgeMarital, aes(x=AgeGroup, y=ConversionRate, fill=Marital))+
  geom_bar(width=0.5, stat="identity",position="stack")+
  ylab("Conversion Rate (%")+
  xlab("Age")+
  ggtitle("Conversion Rates by Age and Marital Status")+
  theme(plot.title = element_text(hjust=0.5))
