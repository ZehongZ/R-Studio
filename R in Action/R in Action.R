x<-rnorm(5)

#Sample R session
age<-c(1,3,5,2,11,9,3,9,12,3)
weight<-c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.2)
mean(weight)
sd(weight)
cor(age,weight)
plot(age,weight)

#Vector
a<-c(1,2,5,3,6,-2,4)
b<-c("one","two","three")
c<-c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE)
print(a)
print(b)
print(c)
a<-c("k","j","h","a","c","m")
a[3]
a[c(1,3,5)]
a[2:6]

#Matrices
y<-matrix(1:20,nrow=5,ncol=4)
print(y)
cells<-c(1,26,24,68)
print(cells)
rnames<-c("R1","R2")
print(rnames)
cnames<-c("C1","C2")
print(cnames)
mymatrix<-matrix(cells,nrow = 2,byrow = TRUE, dimnames = list(rnames,cnames))
print(mymatrix)
mymatrix2<-matrix(cells,nrow = 2,ncol=2,byrow = FALSE, dimnames = list(rnames,cnames))
print(mymatrix2)
x=matrix(1:10,nrow = 2)
print(x)
x[2,]
x[,2]
x[1,4]
x[1,c(4,5)]

#Arrays
dim1<-c("A1","A2")
dim2<-c("B1","B2","B3")
dim3<-c("C1","C2","C3","C4")
z<-array(1:24,c(2,3,4),dimnames = list(dim1,dim2,dim3))
print(z)

#Data frame
patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
diabetes<-c("Type1","Type2","Type1","Type1")
status<-c("Poor","Improved","Excellent","Poor")
patientdata<-data.frame(patientID,age,diabetes,status)
print(patientdata)
patientdata[1:2]
patientdata[c("diabetes","status")]
patientdata$age
table(patientdata$diabetes,patientdata$status)

#Attach, Detach and With
data("mtcars")
summary(mtcars$mpg)
plot(mtcars$mpg,mtcars$disp)
plot(mtcars$mpg, mtcars$wt)

attach(mtcars)
summary(mpg)
plot(mpg,disp)
plot(mpg, wt)
detach(mtcars)

with(mtcars,{
  print(summary(mpg))
  plot(mpg,disp)
  plot(mpg, wt)
})

patientdata2=data.frame(patientID, age, diabetes, status, row.names = patientID)
print(patientdata2)

#Factors
diabetes<-c("Type1","Type2","Type1","Type1")
print(diabetes)
diabetes<-factor(diabetes)
print(diabetes)
status<-c("Poor","Improved","Excellent","Poor")
status<-factor(status, ordered = TRUE, levels=c("Poor","Improved","Excellent"))
status
patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
diabetes<-c("Type1","Type2","Type1","Type1")
status<-c("Poor","Improved","Excellent","Poor")
diabetes<-factor(diabetes)
status<-factor(status, order=TRUE)
patientdata<-data.frame(patientID, age, diabetes, status)
patientdata
str(patientdata)
summary(patientdata)

#Lists
g<-"My first List"
h<-c(25,26,18,39)
j<-matrix(1:10,nrow=5)
k<-c("One","Two","Three")
mylist<-list(title=g, ages=h,j,k)
mylist
mylist[[2]]
mylist[2]
mylist["ages"]
mylist[["ages"]]

#Entering data from the keyboard
mydata<-data.frame(age=numeric(0), gender=character(0), weight=numeric(0))
print(mydata)

#Importing data from a delimited text file
grades<-read.table("studentgrades.csv",header=TRUE, row.names="StudentID", sep = ",")

#Variable labels
names(patientdata)[2]<-"Age at hospitalization (in years)"
patientdata[2]

#Value labels
patientdata$gender<-factor(patientdata$gender, levels=c(1,2), labels=c("male","female"))

#Working with graphs
data("mtcars")
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
detach(mtcars)

#Save the graph as a PDF document
pdf("mygraph.pdf")
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
detach(mtcars)
dev.off()

dose<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
plot(dose,drugA, type="b") #Type b indicates that both points and lines should be plotted.

#Graphical parameters
opar=par(no.readonly = TRUE)
par(lty=2,pch=17)
plot(dose, drugA, type="b")
par(opar)

#Colors
library(RColorBrewer)
n<-7
mycolors<-brewer.pal(n, "Set1")
barplot(rep(1,n),col=mycolors)

n<-10
mycolors<-rainbow(n)
pie(rep(1,n),labels = mycolors,col = mycolors)
mygrays<-gray(0:n/n)
pie(rep(1,n),labels = mygrays, col=mygrays)

#Text characteristics

#Graph and margin dimensions
dose<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
opar<-par(no.readonly = TRUE)
par(pin=c(2,3))#pin: plot dimension in inches
par(lwd=2, cex=1.5)
par(cex.axis=.75, font.axis=3)
plot(dose, drugA, type="b", pch=19, lty=2, col="red")
plot(dose, drugB, type="b", pch=23, lty=6, col="blue", bg="green")
par(opar)

#Adding text, customized axes, and legends
plot(dose, drugA, type="b",col="red",lty=2,pch=2, lwd=2, main="Clinical Trials for Drug A", sub="This is a hypothetical data", xlab="Dosage",ylab="Drug Response",xlim=c(0,60),ylim=c(0,70))

#Titles
title(main="My Titile", col.main="red",sub = "My Subtitle", col.sub="blue", xlab="My X label", ylab = "My Y label",col.lab="green", cex.lab=0.75)

#Axes
x<-c(1:10)
y<-x
z<-10/x
opar<-par(no.readonly = TRUE)
par(mar=c(5,4,4,8)+0.1)
plot(x, y , type="b",pch=21, col="red",yaxt="n",lty=3,ann=FALSE)
lines(x,z, type="b",pch=22, col="blue",lty=2)
axis(2,at=x, labels=x, col.axis="red",las=2)
axis(4, at=z, labels = round(z, digits=2),col.axis="blue",las=2,cex.axis=0.7,tck=-0.1)
mtext("y=1/x", side=4, line=3, cex.lab=1, las=2, col="blue")
title("An Example of Creative Axes", xlab="X values",ylab="Y=X")
par(opar)

#Comparing drugA and drugB response by dose
dose<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
opar<-par(no.readonly = TRUE)
par(lwd=2,cex=1.5,font.lab=2)
plot(dose,drugA, type="b",pch=15,lty=1,col="red",ylim = c(0,60),main="Drug A vs. Drug B",xlab="Drug Dosage",yllab="Drug Response")
lines(dose, drugB, type="b",pch=17,lty=2,col="blue")
abline(h=c(30),lwd=1.5,lty=2,col="grey")
library(Hmisc)
minor.tick(nx=3, ny=3, tick.ratio = 0.5)
legend("topleft", inset=.05, title="Drug Type",c("A","B"), lty=c(1,2),pch=c(15,17),col=c("red","blue"))
par(opar)

#Text annotations
attach(mtcars)
plot(wt, mpg, main="Mileage vs. Car weight", xlab = "Weight", ylab = "Mileage", pch=18, col="blue")
text(wt, mpg, row.names(mtcars), cex=0.6, pos=4,col="red")
detach(mtcars)

opar<-par(no.readonly = TRUE)
par(cex=1.5)
plot(1:7,1:7,type="n")
text(3,3,"Example of default text")
text(4,4, family="mono", "Example of serif text")
text(5,5, family="serif","Example of serif text")
par(opar)

#Combining graphs
attach(mtcars)
opar<-par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(wt, mpg, main="Scatterplot of wt vs. mpg")
plot(wt, disp, main="Scatterplot of wt vs. disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")
par(opar)
detach(mtcars)

attach(mtcars)
opar<-par(no.readonly = TRUE)
par(mfrow=c(3,1))
hist(wt)
hist(mpg)
hist(disp)
par(opar)
detach(mtcars)

attach(mtcars)
layout(matrix(c(1,1,2,3),2,2,byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

attach(mtcars)
layout(matrix(c(1,1,2,3),2,2,byrow = TRUE),widths = c(3,1),heights = c(1,2))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

#Creating a figure arrangement with fine control
##Fine placement of figures ina graph
opar<-par(no.readonly = TRUE)
par(fig=c(0,0.8,0,0.8))
plot(mtcars$wt,mtcars$mpg,xlab="Miles Per Gallon",ylab = "Car Weight")
par(fig=c(0,0.8,0.55,1),new=TRUE)
boxplot(mtcars$wt,horizontal = TRUE, axes=FALSE)
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(mtcars$mpg,axes=FALSE)
mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3)
par(opar)

#Basic data management#

#Creating the leadership data frame
manager<-c(1,2,3,4,5)
date<-c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
country<-c("US","US","UK","UK","UK")
gender<-c("M","F","F","M","F")
age<-c(32,45,25,39,99)
q1<-c(5,3,3,3,2)
q2<-c(4,5,5,4,2)
q3<-c(5,2,5,4,1)
q4<-c(5,5,5,NA,2)
q5<-c(5,5,2,NA,1)
leadership<-data.frame(manager,date,country,gender,age,q1,q2,q3,q4,q5,stringsAsFactors = FALSE)
head(leadership)

#Creating new variables
mydata<-data.frame(x1=c(2,2,6,4),x2=c(3,4,2,8))
mydata$sumx<-mydata$x1+mydata$x2
mydata$meanx<-(mydata$x1+mydata$x2)/2
attach(mydata)
mydata$sumx<-mydata$x1+mydata$x2
mydata$meanx<-(mydata$x1+mydata$x2)/2
detach(mydata)
mydata<-transform(mydata,sumx=x1+x2,meanx=(x1+x2)/2)
head(mydata)

#Recoding variables 
head(leadership)
leadership$age[leadership$age==99]<-NA
leadership$agecat[leadership$age>75]<-"Elder"
leadership$agecat[leadership$age>=55 & leadership$age<=75]<-"Middle Aged"
leadership$agecat[leadership$age<55]<-"Young"
head(leadership)

leadership<-within(leadership,{agecat<-NA
                               agecat[age>75]<-"Elder"
                               agecat[age>=55 & agecat<=75]<-"Middle Aged"
                               agecat[age<55]<-"Young"})

#Renaming variables
names(leadership)[2]<-"TestDate"
head(leadership)

names(leadership)[6:10]<-c("item1","item2","item3","item4","item5")
head(leadership)

#Missing values
y<-c(1,2,3,NA)
is.na(y)
is.na(leadership[,6:10])

#Recoding values to missing
leadership$age[leadership$age==99]<-NA

#Excluding missing values from analyses
x<-c(1,2,NA,3)
y<-sum(x,na.rm = TRUE)#Most numeric functions have an na.rm=TRUE option that removes missing values 
print(y)

leadership
newdata<-na.omit(leadership)#na.omit() deletes any rows with missing data

#Date values
mydates<-as.Date(c("2007-06-22","2004-02-13"))
mydates
strDates<-c("01/05/1965","08/16/1975")
dates<-as.Date(strDates,"%m/%d/%y")
dates

myformat<-"%m/%d/%y"
leadership$TestDate<-as.Date(leadership$TestDate,myformat)
print(leadership$TestDate)

today<-Sys.Date()
today
format(today, format="%A")

startdate<-as.Date("2004-02-13")
enddate<-as.Date("2011-01-22")
days<-enddate-startdate
days

today<-Sys.Date()
dob<-as.Date("1965-10-12")
difftime(today, dob, units = "weeks")

#Converting dates to character variables
strDates<-as.character(dates)
strDates

#Converting from one data type to another
a<-c(1,2,3)
a
is.numeric(a)
is.vector(a)
a<-as.character(a)
a
is.numeric(a)
a
is.vector(a)
is.character(a)

#Sorting data
newdata<-leadership[order(leadership$age),]
attach(leadership)
newdata<-leadership[order(gender,-age),]
detach(leadership)

#Merging datasets
#cbind() joining two matrices or data frames horizontally and don't need to specify a common key
#rbind() to join two data frames vertically
d1=data.frame(id=c(1,2,3),grades=c(10,11,12))
head(d1)
d2=data.frame(id=c(4,5,6),grades=c(13,14,15))
head(d2)
rbind(d1,d2)
cbind(d1,d2)

#Subsetting datasets
newdata<-leadership[,c(6:10)]
head(newdata)

mvars<-c("q1","q2","q3","q4","q5")
newdata=leadership[mvars]
head(newdata)

myvars<-paste("q",1:5,sep="")
newdata=leadership[myvars]
head(newdata)

#Excluding variables
myvars<-names(leadership) %in% c("q3","q4")
newdata<-leadership[!myvars]#Excluding q3, q4
head(newdata)

newdata<-leadership[c(-8,-9)]#(-) sign excludes columns
head(newdata)

#Selecting observations
newdata<-leadership[1:3,]
head(newdata)
newdata<-leadership[leadership$gender=="M"&leadership$age>30,]
head(newdata)

leadership$date<-as.Date(leadership$date,"%m/%d/%y")
startdate<-as.Date("2009-01-01")
enddate<-as.Date("2009-10-31")
newdata<-leadership[which(leadership$date>=startdate & leadership$date<=enddate),]
head(newdata)

#Subset function
newdata<-subset(leadership,age>=35|age<24, select=c(q1,q2,q3,q4))#Select all rows that have a value of age greater than or equal to 35 or less than 24. Keeps variables q1 through q4
print(newdata)

newdata<-subset(leadership, gender=="M" & age>25, select=gender:q4)
print(newdata)

#Random samples
mysample<-leadership[sample(1:nrow(leadership),4,replace=FALSE),]
mysample

#Using SQL statements to manipulate data frames
library(sqldf)
newdf<-sqldf("select*from mtcars where carb=1 order by mpg", row.names=TRUE)
newdf

sqldf("select avg(mpg) as avf_mpg, avg(disp) as avg_disp, gear from mtcars where cyl in (4,6) group by gear")

#Mathematical functions
sqrt(c(4,16,25))

#Statistical functions
x<-c(1,2,3,4,5,6,7,8)
mean(x)
sd(x)

n<-length(x)
meanx<-sum(x)/n
css<-sum((x-meanx)^2)
sdx<-sqrt(css/(n-1))
meanx
sdx

#Generating data from a multivariate normal distribution
library(MASS)
options(digits = 3)
set.seed(1234)
mean<-c(230.7, 146.7, 3.6)
sigma<-matrix(c(15360.8, 6721.2, -47.1, 6721.2, 4700.9, -16.5, -47.1, -16.5, 0.3), nrow=3, ncol=3)
mydata<-mvrnorm(500, mean, sigma)
mydata<-as.data.frame(mydata)
head(mydata)
names(mydata)<-c("y","x1","x2")
dim(mydata)

#Applying functions to data objects
a<-5
sqrt(a)
b<-c(1.243,5.654,2.99)
round(b)
c<-matrix(runif(12),nrow=3)
log(c)
mean(c)

#Applying a function to the rows of a matrix
mydata<-matrix(runif(30),nrow=6)
mydata
apply(mydata,1,mean)
apply(mydata,2,mean)

#Data-management challenge
options(digits = 2)
Student<-c("John Davis","Angela Williams","Bullwinkle Moose","David Jones","Janice Markhammer","Cheryl Cushing","Reuven Ytzrhak","Greg Knox","Joel England","Mary Rayburn")
Math<-c(502,600,412,358,495,512,410,625,573,522)
Science<-c(95,99,80,82,75,85,80,95,89,86)
English<-c(25,22,18,15,20,28,15,30,27,18)
roster<-data.frame(Student, Math, Science, English, stringsAsFactors = FALSE)
head(roster)
z<-scale(roster[,2:4])
head(z)
score<-apply(z,1,mean)
head(score)
roster<-cbind(roster,score)
head(roster)
y<-quantile(score, c(.8,.6,.4,.2))
head(y)
roster$grade[score>=y[1]]<-"A"
roster$grade[score<y[1]&score>=y[2]]<-"B"
roster$grade[score<y[2]&score>=y[3]]<-"C"
roster$grade[score<y[3] & score>=y[4]]<-"D"
roster$grade[score<y[4]]<-"F"
head(roster)

name<-strsplit((roster$Student)," ")
Lastname<-sapply(name, "[",2)
Firstname<-sapply(name,"[",1)
roster<-cbind(Firstname, Lastname, roster[,-1])
roster<-roster[order(Lastname, Firstname),]
roster

#For Loop
for (i in 1:10) print("Hello")

#While Loop
i<-10
while(i>0) {print("Hello");i<-i-1}

#IF-ELSE
grade<-roster$grade
if (is.character(grade)) grade<-as.factor(grade)
if (!is.factor(grade)) grade<-as.factor(grade) else print("Grade alread is a factor")

#IFELSE
ifelse(score>0.5, print("Passed"), print("Failed"))
outcome<-ifelse (score>0.5,"Passed","Failed")

#SWITCH
feelings<-c("sad","afraid")
for (i in feelings)
  print(
    switch (i,
      happy="I am glad you are happy",
      afraid="There is nothing to fear",
      sad="Cheer up",
      angry="Calm down now"
    )
  )

#Transpose
cars<-mtcars[1:5,1:4]
cars
t(cars)

#Aggregating data
options(digits=3)
attach(mtcars)
aggdata<-aggregate(mtcars, by=list(cyl, gear), FUN=mean, na.rm=TRUE)
aggdata

#Reshape2 package
library(reshape2)
md<-melt(mydata, id=c("ID","Time"))#When you melt a dataset, you restructure it into a format in which each measured vari- able is in its own row along with the ID variables needed to uniquely identify it.
md

#Bar plots
library(vcd)
head(Arthritis)
barplot(Arthritis$Age)

#Simple bar plots
library(vcd)
counts<-table(Arthritis$Improved)
counts
barplot(counts, main="Simple Bar Plot", xlab = "Improvement", ylab = "Frequency")

#Horizontal bar plot
barplot(Arthritis$Improved, main = "Horizontal Bar Plot", xlab = "Frequency","Improvement", horiz = TRUE)

#Stacked and grouped bar plots
library(vcd)
counts<-table(Arthritis$Improved, Arthritis$Treatment)
counts

#Stacked bar plot
barplot(counts, 
        main="Stacked Bar Plot",
        xlab="Treatment",ylab="Frequency",
        col=c("red","yellow","green"),
        legend=rownames(counts))

#Grouped bar plot
barplot(counts,
        main="Grouped Bar Plot",
        xlab="Treatment",ylab = "Frequency",
        col=c("red","yellow","green"),
        legend=rownames(counts), beside = TRUE)

#Mean bar plots
states<-data.frame(state.region, state.x77)
head(states)
means<-aggregate(states$Illiteracy, by=list(state.region), FUN=mean)
means
barplot(means$x, names.arg = means$Group.1)
title("Mean Illiteracy Rate")

#Tweaking bar plots
par(mar=c(5,8,4,2))
par(las=2)
counts<-table(Arthritis$Improved)
barplot(counts,
        main="Treatment Outcome",
        horiz=TRUE,
        cex.names = 0.8,
        names.arg = c("No Improvement","Some Improvement","Marked Improvement"))

#Spinograms
library(vcd)
attach(Arthritis)
counts<-table(Treatment, Improved)
spine(counts, main = "Spinogram Example")
detach(Arthritis)

#Pie Charts
par(mfrow=c(2,3))
slices<-c(10,12.4,16,8)
lbls<-c("US","UK","Australia","Germany","France")
pie(slices, labels=lbls,main="Simple Pie Chart")

pct<-round(slices/sum(slices)*100)
lbls2<-paste(lbls, " ",pct,"%",sep="")
pie(slices, labels=lbls,col=rainbow(length(lbls2)), main="Pie Chart with Percentages")

library(plotrix)
pie3D(slices, labels=lbls,explode = 0.1,main="3D Pie Chart")
mytable<-table(state.region)
lbls3<-paste(names(mytable),"\n",mytable, sep="")
pie(mytable, labels = lbls3, main="Pie Chart from a Table with sample sizes")

#Fan Plot
library(plotrix)
slices<-c(10,12,4,16,8)
lbls<-c("US","UK","Australia","Germany","France")
fan.plot(slices, labels = lbls, main="Fan Plot")

#Histogram
par(mfrow=c(2,2))
hist(mtcars$mpg)
hist(mtcars$mpg,
     breaks = 12,
     col="red",
     xlab="Miles Per Gallon",
     main="Colored histogram with 12 bins")

#Histogram with a rug plot
hist(mtcars$mpg,
     freq = FALSE,
     breaks=12,
     col="red",
     xlab = "Miles Per Gallon",
     main="Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg),col="blue",lwd=2)

#Histogram with a normal curve and frame
x<-mtcars$mpg
h<-hist(x,
        breaks=12,
        col="red",
        xlab="Miles Per Gallon",
        main="Histogram with normal curve and box")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit, mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2])*length(x)
lines(xfit,yfit,col="blue",lwd=2)
box()

#Kernel density plots
par(mfrow=c(2,1))
d<-density(mtcars$mpg)
plot(d)
d<-density(mtcars$mpg)
plot(d,main="Kernel Density of Miles Per Gallon")
ploygon(d, col="red",border="blue")
rug(mtcars$mpg,col="brown")
par()

#Comparative Kernel Density Plots
library(sm)
attach(mtcars)
cyl.f<-factor(cyl, levels = c(4,6,8), labels = c("4 cylinder","6 cylinder","8 cylinder"))
sm.density.compare(mpg,cyl, xlab="Miles per gallon")
title(main="MPG Distribution by Car Cylinders")
colfill<-c(2:(1+length(levels(cyl.f))))
legend(locator(1), levels(cyl.f), fill=colfill)
detach(mtcars)

#Boxplot
boxplot(mtcars$mpg,main="Box Plot",ylab="Miles Per Gallon")

#Parallel box plots
boxplot(mpg~cyl, data=mtcars,
        main="Car Mileage Data",
        xlab="Number of Cylinders",
        ylab="Miles Per Gallon")

#Box plots for two crossed factors
mtcars$cyl.f<-factor(mtcars$cyl,levels = c(4,6,8),labels=c("4","6","8"))
mtcars$am.f<-factor(mtcars$am,levels = c(0,1),labels=c("auto","standard"))
boxplot(mpg~am.f*cyl.f,
        data=mtcars,
        varwidth=TRUE,
        col=c("gold","darkgreen"),
        main="MPG Distribution by Auto Type",
        xlab="Auto Type", ylab="Miles Per Gallon")

#Violin plots
library(vioplot)
library(sm)
x1<-mtcars$mpg[mtcars$cyl==4]
x2<-mtcars$mpg[mtcars$cyl==6]
x3<-mtcars$mpg[mtcars$cyl==8]
vioplot(x1,x2,x3,
        names=c("4 cyl","6 cyl","8 cyl"),
        col="gold")

#Dot plots
dotchart(mtcars$mpg, labels=row.names(mtcars),cex=.7,main="Gas Mileage for Car Models",xlab = "Miles Per Gallon")

#Dot plot grouped, sorted and colored
x<-mtcars[order(mtcars$mpg),]
x$cyl<-factor(x$cyl)
x$color[x$cyl==4]<-"red"
x$color[x$cyl==6]<-"blue"
x$color[x$cyl==8]<-"darkgreen"
dotchart(x$mpg,
         labels=row.names(x),
         cex=.7,
         groups=x$cyl,
         gcolor="black",
         color=x$color,
         pch=19,
         main="Gas Mileage for Car Models by cylinder",
         xlab="Miles Per Gallon")

#Descriptive Statistics
myvars<-c("mpg","hp","wt")
head(mtcars[myvars])

#Descriptive statistics via sapply()
mystats<-function(x, na.omit=FALSE){
  if (na.omit)
    x<-x[!is.na(x)]
  m<-mean(x)
  n<-length(x)
  s<-sd(x)
  skew<-sum((x-m)^3/s^3)/n
  kurt<-sum((x-m)^4/s^4)/n-3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}
myvars<-c("mpg","hp","wt")
sapply(mtcars[myvars], mystats)

#Descriptive statistics via describe()
library(Hmisc)
myvars<-c("mpg","hp","wt")
describe(mtcars[myvars])

#Descriptive statistics via stat.desc()
library(pastecs)
myvars<-c("mpg","hp","wt")
stat.desc(mtcars[myvars])

#Descriptive statistics via describe()
library(psych)
myvars<-c("mpg","hp","wt")
describe(mtcars[myvars])

#Descriptive statistics by group
myvars<-c("mpg","hp","wt")
aggregate(mtcars[myvars],by=list(am=mtcars$am),mean)
aggregate(mtcars[myvars],by=list(am=mtcars$am),sd)

#Descriptive statistics by group using by ()
dstats<-function(x)sapply(x,mystats)
myvars<-c("mpg","hp","wt")
by(mtcars[myvars],mtcars$am,dstats)

#Descriptive statistics by group using summaryBy()
library(doBy)
summary(mpg+hp+wt~am, data=mtcars, FUN=mystats)

#Summary statistics by group using describe.by()
library(psych)
myvars<-c("mpg","hp","wt")
describeBy(mtcars[myvars],list(am=mtcars$am))

#One-way frequency tables
library(vcd)
mytable<-with(Arthritis,table(Improved))
mytable

prop.table(mytable)*100

#Two-way frequency tables
mytable<-xtabs(~Treatment+Improved, data=Arthritis)
mytable
margin.table(mytable,1)
prop.table(mytable,1)
margin.table(mytable,2)
prop.table(mytable,2)
prop.table(mytable)
addmargins(mytable)
addmargins(prop.table(mytable))
addmargins(prop.table(mytable,1),2)

#Two-way table using CrossTable
library(gmodels)
CrossTable(Arthritis$Treatment,Arthritis$Improved)

#Three-way contingency table
mytable<-xtabs(~Treatment+Sex+Improved, data=Arthritis)
mytable
ftable(mytable)
margin.table(mytable,2)
margin.table(mytable,3)
margin.table(mytable,c(1,3))
ftable(prop.table(mytable, c(1,2)))
ftable(addmargins(prop.table(mytable,c(1,2)),3))

#Chi-square test of independence
library(vcd)
mytable<-xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)

mytable<-xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)

#Fisher's Exact Test
mytable<-xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)
mytable

#Cochran-Mantel-Haenszel Test
mytable<-xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)

#Measures of association for a two-way table
library(vcd)
mytable<-xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)

#Covariances and Correlations
library(vcd)
states<-state.x77[,1:6]
head(states)
cov(states)
cor(states)

x<-states[,c("Population","Income","Illiteracy","HS Grad")]
y<-states[,c("Life Exp","Murder")]
cor(x,y)

#Partial correlations
library(ggm)
colnames(states)
pcor(c(1,5,2,3,6),cov(states))

#Testing a correlation coefficient for significance
cor.test(states[,3],states[,5])

#Correlation matrix and tests of significance via corr.test()
library(psych)
corr.test(states, use="complete")

#Independent t-test
library(MASS)
t.test(Prob~ So, data=UScrime)#Reject the hypothesis that Southern states and non-Southern states have equal probabilities of imprisonment

#Dependent t-test
#A dependent t-test assumes that the difference between groups is normally distributed
library(MASS)
sapply(UScrime[c("U1","U2")],function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime, t.test(U1,U2,paired=TRUE))
#The mean difference is 61.5, which is large enough to warrant rejection of the hypothesis that the mean upemployment rate for older and younger males is the same

#Comparing two groups
with(UScrime, by(Prob, So, median))
wilcox.test(Prob~So, data=UScrime)
sapply(UScrime[c("U1","U2")],median)
with(UScrime, wilcox.test(U1,U2, paired = TRUE))

#Compaing more than two groups
states<-data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~ state.region, data=states)

#Nonparametric multiple comparisons
library(wavemulcor)
library(W2CWM2C)
library(colorspace)
states<-data.frame(state.region, state.x77)
wmc(Illiteracy~state.region, data=states, method="holm")

#Regression
#Normality, Independce, Linearity, Homoscedasticity (constant varaince)
#Simple linear regression
head(women)
fit<-lm(weight~height, data=women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)
plot(women$height,women$weight,xlab="Height",ylab="Weight")
abline(fit)

#Polynomial regression
head(women)
fit2<-lm(weight~height+I(height^2),data=women)
summary(fit2)
plot(women$height,women$weight,xlab="Height",ylab="weight")
lines(women$height,fitted(fit2))

#3rd degree polynomial regression
fit3<-lm(weight~height+I(height^2)+I(height^3),data=women)
library(car)
scatterplot(weight~height, data=women, spread=FALSE, smoother.args=list(lty=2), pch=19, main="Women Age 30-39", xlab="Height",ylab="Weight")

#Examining bivariate relationships
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
cor(states)

#Multiple linear regression
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
summary(fit)

#Multiple linear regression with interactions
fit<-lm(mpg~hp+wt+hp*wt, data=mtcars)

#Regression diagnostics
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
confint(fit)

#A typical approach
fit<-lm(weight~height, data=women)
par(mfrow=c(2,2))
plot(fit)
summary(fit)

fit2<-lm(weight~height+I(height^2), data=women)
par(mfrow=c(2,2))
plot(fit2)
summary(fit2)

newfit<-lm(weight~height+I(height^2), data=women[-c(13,15),])

fit3<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
par(mfrow=c(2,2))
plot(fit3)
summary(fit3)

#Normality
library(car)
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
qqplot(fit, labels=row.names(states),id.method="identify", simulate=TRUE, main="Q-Q Plot")
states["Nevada",]#Murder=11.5
fitted(fit)['Nevada']#But prediction is 3.9%
residuals(fit)['Nevada']
rstudent(fit)['Nevada']

#Independence of errors
durbinWatsonTest(fit)

#Linearity
library(car)
crPlots(fit)

#Homoscedasticity
library(car)
ncvTest(fit)
spreadLevelPlot(fit)

#Linear model assumption
library(gvlma)
gvmodel<-gvlma(fit)
summary(gvmodel)

#Multicollinearity (sqrt(vif)>2 inidicates a problrm)
library(car)
vif(fit)
sqrt(vif(fit))>2

#Outlier test
library(car)
outlierTest(fit)
#Nevada is an outlier since Bonferonni p <0.05

#High-leverage points
hat.plot<-function(fit){
  p<-length(coefficients(fit))
  n<-length(fitted(fit))
  plot(hatvalues(fit),main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n,col="red",lty=2)
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))
}
hat.plot(fit)

#Influential observations
cutoff<-4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")

library(car)
avPlots(fit, ask=FALSE, id.method="identify")

library(car)
influencePlot(fit, id.method="identify", main="Influence Plot",sub="Circle size is proportional to Cook's distance")

#Box-cox transformation
library(car)
summary(powerTransform(states$Murder))
boxTidwell(Murder~Population+Illiteracy, data=states)

#Comparing models using the anova() function
states<-as.data.frame(state.x77[,c("Murder","Population", "Illiteracy","Income","Frost")])
fit1<-lm(Murder~ Population+Illiteracy+Income+Frost, data=states)
fit2<-lm(Murder~Population+Illiteracy, data=states)
anova(fit2,fit1)#0.9939 nonsignificant, drop income and frost

#Comparing models with the AIC
fit1<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
fit2<-lm(Murder~Population+Illiteracy, data=states)
AIC(fit1,fit2)

#Backward stwpwise selection
library(MASS)
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
stepAIC(fit, direction="backward" )

#All subsets regression
library(leaps)
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
leaps<-regsubsets(Murder~Population+Illiteracy+Income+Frost, data=states, nbest=4)
plot(leaps, scale="adjr2")
library(car)
subsets(leaps, statistic = "cp",main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="red")

#Analysis of variance
