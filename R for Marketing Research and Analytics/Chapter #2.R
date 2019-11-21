#Install libraries
library(lavaan)
library(corrplot)
library(multcomp)
#Load dataset
setData<-read.csv("satData.csv", header = T)
str(setData)
setData$Segment<-factor(setData$Segment)
head(setData)
summary(setData)
#Plot the correlation matrix, omitting the categorical Segment
corrplot.mixed(cor(setData[c("X","iProdSAT","iProdREC","iSalesREC")]))
#Does product satisfaction differ by segment?
aggregate(iProdSAT~Segment, setData, mean)
#See if satisfaction differs significantly by segment
sat.anova<-aov(iProdSAT~-1 + Segment, setData)
summary(sat.anova)
#Plot the ANOVA model
par(mar=c(4,8,4,2))
plot(glht(sat.anova))
satModel<-"SAT=~iProdSAT+iSalesSAT
REC=~iProdREC+iSalesREC
REC~SAT"
sat.fit<-cfa(satModel, data=setData)
summary(sat.fit, fit.m=TRUE)

#2.4.3
#Vector
x<-c(2,4,6,8)
xNum<-c(1,3.14159, 5, 7)
xLog<-c(TRUE, FALSE, TRUE, TRUE)
xChar<-c("foo","bar","boo","far")
xMix<-c(1, TRUE, 3, "Hello, world!")
#Vector can be appended to one another
x2<-c(x,x)
x2
#Mathematical
x2+1
x2*pi
(x+cos(0.5))*x2
#More on Vectors and Indexing
xSeq<-1:10
xSeq
1:5*2
1:(5*2)
#Sequence & Replication
seq(from=-5, to=28, by=4)
rep(c(1,2,3), each=3)
xNum
xNum[c(FALSE, TRUE, TRUE, TRUE)]

#2.4.5
#Missing and Interesting Values
my.test.scores<-c(91, NA, NA)
#Ignore NA data
mean(my.test.scores, na.rm = TRUE)
max(my.test.scores, na.rm = TRUE)
#Remove NA data
mean(na.omit(my.test.scores))
#is.na
is.na(my.test.scores)
my.test.scores[!is.na(my.test.scores)]
#log
log(c(-1,0,1))

#2.4.6
str(xNum)
str(xChar)
xList<-list(xNum, xChar)
xList[1]
xList[2]
str(xList)
summary(xList[[1]])
lapply(xList, summary)
#Assign names method 1
xList<-list(xNum, xChar)
names(xList)<-c("itemnum","itemchar")
xList
#Assign names method 2
xList<-list(itemnum=xNum, itemchar=xChar)
names(xList)
#Data Frames
x.df<-data.frame(xNum, xLog, xChar)
x.df
#Data Frames indexing
x.df[2,1]
x.df[1,3]
#Prevent conversion to factors
x.df<-data.frame(xNum, xLog, xChar, stringsAsFactors = FALSE)
x.df
x.df[1,3]
#Create a new data set that is more representative of data
rm(list=ls())
store.num<-factor(c(3,14,21,32,54))
store.rev<-c(543, 654, 345, 678, 234)
store.visits<-c(45, 78, 32, 56, 34)
store.manager<-c("Annie", "Bert", "Carla", "Dave", "Ella")
store.df<-data.frame(store.num, store.rev, store.visits, store.manager, stringsAsFactors = F)
store.df
store.df$store.manager
cor(store.df$store.rev, store.df$store.visits)
summary(store.df)

#2.6
save(store.df, file="store-df-back.RData")
rm(store.df)
mean(store.df$store.rev)
load("store-df-back.RData")
mean(store.df$store.rev)

#2.62 CSV Files
write.csv(store.df, row.names = FALSE)
#Write the file then read it
write.csv(store.df, file="store-df.csv", row.names = FALSE)
read.csv("store-df.csv")
#Assign a file to an object
store.df2<-read.csv("store-df.csv", stringsAsFactors = FALSE)
store.df2$store.num<-factor(store.df2$store.num)
store.df==store.df2#Test whether the two data frames are the same
all.equal(store.df, store.df2)

#2.7 Writting Your Own Functions
#Declare a function
se<-function(x) {sd(x)/sqrt(length(x))}
se(store.df$store.visits)

