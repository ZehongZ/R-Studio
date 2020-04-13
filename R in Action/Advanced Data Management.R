#Numerical and character functions
#Calculating the mean and standard deviation
x<-c(1,2,3,4,5,6,7,8)
mean(x)
sd(x)
#Generating pseudi-random numbers from a uniform distribution
runif(5)
runif(5)
set.seed(1234)
runif(5)
set.seed(1234)
runif(5)
#Generating data from a multivariate normal distribution
library(MASS)
options(digits=3)
set.seed(1234)
mean<-c(230.7, 146.7, 3.6)
sigma<-matrix(c(15360.8, 6721.2, -47.1, 
                6721.2, 4700.9, -16.5,
                -47.1, -16.5, 0.3), nrow=3, ncol=3)
mydata<-mvrnorm(500, mean, sigma)
mydata<-as.data.frame(mydata)
names(mydata)<-c('y','x1','x2')
dim(mydata)
head(mydata, 10)
#Applying functions to data objects
a<-5
sqrt(a)
b<-c(1.243, 5.654, 2.99)
round(b)
c<-matrix(runif(12),nrow=3)
c
log(c)
mean(c)
#Applying a function to the rows of a matrix
mydata<-matrix(rnorm(30), nrow=6)
mydata
apply(mydata,1,mean)
apply(mydata,2,mean)
apply(mydata, 2, mean, trim=0.2)
#A solution to the learning example
options(digits=2)
Student<-c('John Davis','Angela William','Bullwinkle Moose','David Jones', 'Janice Markhammer',
           'Cheryl Cushing','Reuven Ytzrhak','Greg Knox','Joel England','Mary Rayburn')
Math<-c(502,600,412,358,495,512,410,625,573,522)
Science<-c(95,99,80,82,75,85,80,95,89,86)
English<-c(25,22,18,15,20,28,15,30,27,18)
roster<-data.frame(Student, Math, Science, English, stringsAsFactors = FALSE)
names(roster)
#Obtain the performance scores
z<-scale(roster[,2:4])
score<-apply(z,1,mean)
roaster<-cbind(roster, score)
#Grades the students
y<-quantile(score, c(.8,.6,.4,.2))
roaster$grade[score>=y[1]]<-'A'
roaster$grade[score<y[1]&score>=y[2]]<-'B'
roaster$grade[score<y[2]&score>=y[3]]<-'C'
roaster$grade[score<y[3]&score>=y[4]]<-'D'
roaster$grade[score<y[4]]<-'F'
#Extracts the last and first name
name<-strsplit((roaster$Student),"")
Lastname<-sapply(name, "[",2)
Firstname<-sapply(name, "[",1)
roster<-cbind(Firstname, Lastname, roster[,-1])
roster<-roster[order(Lastname, Firstname),]
roster
options(digits=2)
roster
z<-scale(roster[,2:4])
z
#Perform scale score for each student
score<-apply(z,1,mean)
roster<-cbind(roster, score)
roster
y<-quantile(roster$score, c(.8,.6,.4,.2))
y
#Recode students' percentile ranks
roster$grade[score>=y[1]]<-'A'
roster$grade[score<y[1]&score>=y[[2]]]<-'B'
roster$grade[score<y[2]&score>=y[[3]]]<-'C'
roster$grade[score<y[3]&score>=y[4]]<-'D'
roster$grade[score<y[4]]<-'g'
#Repetition and looping
#for
for (i in 1:10) print('Hello')
#while
v<-c("Hello",'while loop')
cnt<-2
while (cnt<7) {
  print(v)
  cnt=cnt+1
}
#while #2
i<-10
while (i>0) {
  print('Hello World')
  i=i-1
}
#Conditional execution
#IF-ELSE
ifelse(score>0.5, print('Passed'), print('Failed'))
outcome<-ifelse(score>0.5, 'Passed','Failed')
outcome
#A switch example
feelings<-c('sad','afraid')
for (i in feelings)
  print(
    switch(i,
           happy='I am glad you are happy',
           afraid='There is nothing to fear',
           sad='Cheer up',
           angry='Calm down now'
           )
  )
#User-written functions
mystates<-function(x, parametric=TRUE, print=FALSE){
  if (parametric) {
    center<-mean(x);
    spread<-sd(x)
  } else {
    center<-median(x);
    spread<-mad(x)
  }
  if (print & parametric) {
    cat('Median=', center, "\n", "MAD=", spread, "'n")
  }
  result<-list(center=center, spread=spread)
  return(result)
}
set.seed(1234)
x<-rnorm(500)
y<-mystates(x)
y<-mystates(x, parametric=FALSE, print=TRUE)
y$center
#User-define function 2
mydate<-function (type="long") {
  switch(type,
         long=format(Sys.time(), "%A %B %Y"),
         short=format(Sys.time(),"%m-%d-%y"),
         cat(type, "is not a recognized type\n"))
}
mydate('long')
mydate('short')
mydate()
mydate('medium')

#Aggregation and reshaping
#Transpose a dataset
cars<-mtcars[1:5,1:4]
cars
t(cars)
#Aggregating data
options(digits=3)
attach(mtcars)
head(mtcars)
aggdata<-aggregate(mtcars, by=list(cyl, gear), FUN=mean, na.rm=TRUE)
aggdata
#The reshape2 package
ID=c(1,1,2,2)
Time=c(1,2,1,2)
X1=c(5,3,6,2)
X2=c(6,5,1,4)
mydata=data.frame(ID, Time, X1, X2)
mydata
library(reshape2)
md<-melt(mydata, id=c("ID","Time"))
md
