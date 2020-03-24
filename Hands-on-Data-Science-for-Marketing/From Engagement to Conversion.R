#Import dataset
df<-read.csv("bank-full.csv",header = T, sep=";")
#Overview of dataset
head(df)
summary(df)
str(df)
#Encode conversions as 0s and 1s
df$conversion<-as.integer(df$y)-1
#Conversion Rate
sprintf("Conversion rate:%0.2f%%", sum(df$conversion)/nrow(df)*100)
#Conversion Rates by job
conversionsByJob<-df%>%
  group_by(Job=job)%>%
  summarise(Count=n(), NumConversions=sum(conversion))%>%
  mutate(ConversionRate=NumConversions/Count*100)
conversionsByJob
#Plot data using bar chart
ggplot(conversionsByJob, aes(x=Job, y=ConversionRate))+
  geom_bar(width=0.5, stat="identity")+
  coord_flip()+
  ggtitle("Conversion Rates by Job")+
  xlab("Job")+
  ylab("Conversion Rate(%)")+
  theme(plot.title = element_text(hjust=0.5))
#Default rates by conversions 
defaultByConversion<-df%>%
  group_by(Default=default, Conversion=conversion)%>%
  summarise(Count=n())
#Pie chart to show default rates by conversions
ggplot(defaultByConversion, aes(x="",y=Count, fill=Default))+
  geom_bar(width = 1, stat="identity", position=position_fill())+
  geom_text(aes(x=1.25, label=Count), position=position_fill())+
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust=0.5))+
  coord_polar("y")+
  facet_wrap(~Conversion)+
  ggtitle("Default(0:Non Conversion, 1:Conversion)")+
  theme(
    axis.title.x=element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust=0.5),
    legend.position = "bottom"
  )
###Bank balance by conversions
#Boxplot with outliers
ggplot(df, aes(x="", y=balance))+
  geom_boxplot()+
  facet_wrap(~conversion)+
  ylab("balance")+
  xlab("0:Non-conversion,1:Conversion")+
  ggtitle("Conversion vs. Non-Conversion: Balance")+
  theme(plot.title = element_text(hjust=0.5))
#Boxplot without outliers
ggplot(df, aes(x="",y=balance))+
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits=c(-2000,5000))+
  facet_wrap(~conversion)+
  ylab("balance")+
  xlab("0:Non-Conversion, 1:Conversion")+
  ggtitle("Conversion vs. Non-Conversion Balance")+
  theme(plot.title = element_text(hjust=0.5))
#Conversion rates by number of contacts
conversionsByNumContacts<-df%>%
  group_by(Campaigh=campaign)%>%
  summarise(Count=n(), NumConversions=sum(conversion))%>%
  mutate(ConversionRate=NumConversions/Count*100)
ggplot(conversionsByNumContacts, aes(x=Campaigh, y=ConversionRate))+
  geom_bar(width=0.5, stat="identity")+
  ggtitle("Conversion Rates by Number of Contacts")+
  xlab("Number of Contacts")+
  ylab("Conversion Rate(%")+
  theme(plot.title=element_text(hjust=-0.5))
#Encoding the month
unique(df$month)
#Encode the string values of month with numbers
months=lapply(month.abb, function(x)tolower(x))
df$month<-match(df$month, months)
match(unique(df$month), months)
#See how many records we have for each month
df%>%
  group_by(month)%>%
  summarise(Count=n())
#Encoding the job, housing and marital variables
df$job<-factor(df$job)
df$housing<-factor(df$housing)
df$marital<-factor(df$marital)
#Building decision tress
library(rpart)
fit<-rpart(
  conversion~age+balance+campaign+previous+housing+job+marital,
  method="class",
  data=df,
  control=rpart.control(maxdepth = 4, cp=0.0001)
)
#Visualize decision trees
library(rattle)
fancyRpartPlot(fit)
