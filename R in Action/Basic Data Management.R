#Creating the leadership data frame
manager<-c(1,2,3,4,5)
date<-c('10/24/08','10/28/08','10/1/08','10/12/08','5/1/09')
country<-c('US','US','UK','UK','UK')
gender<-c('M','F','F','M','F')
age<-c(32,45,25,39,99)
q1<-c(5,3,3,3,2)
q2<-c(4,5,5,3,2)
q3<-c(5,2,5,4,1)
q4<-c(5,5,5,NA,2)
q5<-c(5,5,2,NA,1)
leadership<-data.frame(manager, date,country,gender, age,q1,q2,q3,q4,q5,stringsAsFactors = FALSE)

#Creating new variables
mydata<-data.frame(x1=c(2,2,6,4),
                   x2=c(3,4,2,8))
mydata$sumx<-mydata$x1+mydata$x2
mydata$meanx<-(mydata$x1+mydata$x2)/2
attach(mydata)
mydata$sumx<-x1+x2
mydata$meanx<-(x1+x2)/2
detach(mydata)
mydata<-transform(mydata,
                  sumx=x1+x2,
                  meanx=(x1+x2)/2)

#Recoding Variables
leadership$age[leadership$age==99]<-NA
#Create agecat variable
leadership$agecat[leadership$age>75]<-'Elder'
leadership$agecat[leadership$age>=55 & leadership$age<=75]<='Middle Aged'
leadership$agecat[leadership$age<55]<-'Young'
leadership
#Or
leadership<-within(leadership,{
  agecat<-NA
  agecat[age>75]<-'Elder'
  agecat[age>=55 & age<=75]<- 'Middle Aged'
  agecat[age<55]<-'Young'})

#Renaming Variables
#Method #1
fix(leadership)
#Method #2
names(leadership)
names(leadership)<-'testDate'
leadership
names(leadership)[6:10]<-c('item1','item2','item3','item4','item5')
#Method #3
library(plyr)
leadership<-rename(leadership,
                   c(manager='managerID', date='testDate'))

#Missing Values
#Applying the is.na()
is.na(leadership[,6:10])
#Excluding missing values from analyses
x<-c(1,2,NA,3)
y<-x[1]+x[2]+x[3]+x[4]
z<-sum(x)
x<-c(1,2,NA,3)
y<-sum(x,na.rm = TRUE)
y
#Using na.omit() to delete incomplete observations
leadership
newdata<-na.omit(leadership)
newdata

#Date Values
mydates<-as.Date(c('2007-06-22','2004-02-13'))
strDates<-c('01/05/1965','08/16/1975')
dates<-as.Date(strDates, '%m/%d/%Y')
#Or
myformat<-'%m/%d/%y'
dates<-as.Date(strDates, myformat)
#System date
Sys.Date()
date()
#Format date
today<-Sys.Date()
format(today, format='%B %d %Y')
format(today, format='%A')
#Calculate days difference
startdate<-as.Date('2004-02-13')
enddate<-as.Date('2011-01-22')
days<-enddate-startdate
days
#Difftime
today<-Sys.Date()
dob<-as.Date('1956-10-12')
difftime(today, dob, units='days')
#Converting dates to character variables
strDate<-as.character(dates)

#Converting from on data type to another
a<-c(1,2,3)
a
is.numeric(a)
is.vector(a)
a<-as.character(a)
a
is.numeric(a)
is.vector(a)
is.character(a)

#Sorting data
newdata<-leadership[order(leadership$age),]
newdata
##Sorted from youngest manager to oldest manager
attach(leadership)
newdata<-leadership[order(gender, -age),]
detach(leadership)

#Merging datasets

#Substting datasets
#Selecting(keeping) Variables
newdata<-leadership[,c(6:10)]#Method 1
myvars<-c('q1','q2','q3','q4','q5')#Method 2
newdata<-leadership[myvars]
myvars<-paste('q', 1:5, sep="")#Method 3
newdata<-leadership[myvars]
#Dropping variables
myvars<-names(leadership)%in%c('q3','q4')#Method 1
newdata<-leadership[!myvars]

#Sorting observations
#Selecting observations
newdata<-leadership[1:3,]#Method 1
newdata<-leadership[leadership$gender=='M' & leadership$age>30,]
attach(leadership)#Method 2
newdata<-leadership[gender=='M' & age>30,]
detach(leadership)
#Selecting dates
leadership$date<-as.Date(leadership$date,'%m/%d/%y')
startdate<-as.Date("2009-01-01")
enddate<-as.Date("2009-10-31")
newdata<-leadership[which(leadership$date>=startdate & leadership$date<=enddate),]

#The subset() function
newdata<-subset(leadership, age>=35 | age< 24, select=c(q1,q2,q3,q4))
newdata
newdata<-subset(leadership, gender=='M' & age>25, select=gender:q4)
newdata

#Random samples
mysample<-leadership[sample(1:nrow(leadership),3,replace=FALSE),]

#Using SQL Statement to manipulate data frames
library(sqldf)
newdf<-sqldf("select * from mtcars where carb=1 order by mpg",
             row.names=TRUE)
newdf
