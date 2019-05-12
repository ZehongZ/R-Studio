####Vectors####
#Character Vector
subject_name<-c("John Doe", "Jane Doe","Steve Graves")
#Double Vector
temperature<-c(98.1,98.6,101.4)
temperature[2]
temperature[2:3]
temperature[-2]
temperature[c(TRUE,TRUE,FALSE)]
#Logical Vector
flu_status<-c(FALSE,FALSE,TRUE)

####Factor####
#A factor is a special case of vector that is solely used to represent categorical or ordinal variables
gender<-factor(c("MALE","FEMALE","MALE"))
gender
blood<-factor(c("O","AB","A"), levels = c("A","B","AB","O"))
symptoms<-factor(c("SEVERE","MILD","MODERATE"),
                 levels=c("MILD","MODERATE","SEVERE"),
                 ordered=TRUE)
#Test whether each patient's symptoms are greater than moderate
symptoms>"MODERATE"

####List####
subject1<-list(fullname=subject_name[1],
               temperature=temperature[1],
               flu_status=flu_status[1],
               gender=gender[1],
               blood=blood[1],
               symptoms=symptoms[1])
subject1
subject1[2]
subject1$temperature
subject1[c("temperature","flu_status")]

####Data Frames####
pt_data<-data.frame(subject_name, temperature, flu_status,gender,blood, symptoms, stringsAsFactors = FALSE)
pt_data
pt_data$subject_name
pt_data[c("temperature","flu_status")]
pt_data[1,2]#First row, second column
pt_data[c(1,3),c(2,4)]
pt_data[1,]#First row
pt_data[,1]#First column

####Matrixes and Arrays####
m<-matrix(c(1,2,3,4),nrow=2)
m
m<-matrix(c(1,2,3,4),ncol=2)
m

####Measuring the central tendency####
mean(c(36000,44000,56000))
median(c(36000,44000,56000))

####Measuring spread####
data("mtcars")
range(mtcars$mpg)
diff(range(mtcars$mpg))
IQR(mtcars$mpg)#Interquartile Range (IQR): difference between Q1 and Q3
quantile(mtcars$mpg)
quantile(mtcars$mpg,probs = c(0.01,0.99))#1th and 99th percentiles
quantile(mtcars$mpg,seq(from=0,to=1,by=0.2))#Specify quantiles

####Uniform and Normal distributions####
#Standard deviation = spread
#Variance is defined as the average of the squared difference between each value and mean value
#Standard deviation is the square root of the variance
var(mtcars$mpg)
sd(mtcars$mpg)
#While interpreting the variance, larger numbers indicate that the data are spread more widely around the mean
#The standard deviation indicates how much each value differs from the mean

####Exploring categorical variables
table(iris$Species)
prop.table(table(iris$Species))

####Two-way cross-tabulations####
library(gmodels)
CrossTable(x=iris$Sepal.Length,iris$Species)
