#Vector
subject_Name<-c("John Doe","Jane Doe","Steve Graves")
temperature<-c(98.1, 98.6, 101.4)
flu_status<-c(FALSE, FALSE, TRUE)
temperature[2]
temperature[2:3]
temperature[-2]
temperature[c(TRUE,TRUE,FALSE)]

#Factor
gender<-factor(c("MALE","FEMALE","MALE"))
gender
blood<-factor(c("O","AB","A"), levels=c("A","B","AB","O"))
blood[1:2]
symptons<-factor(c("SEVERE","MILD","MODERATE"),levels=c("MILD","MODERATE","SEVERE"),ordered=TRUE)
symptons
symptons>"MODERATE"

#List
subject1<-list(fullname=subject_Name[1],
               temperature=temperature[1],
               flu_status=flu_status[1],
               gender=gender[1],
               blood=blood[1],
               symptons=symptons[1])
subject1
subject1[2]
subject1[[2]]
subject1$temperature
subject1[c("temperature","flu_status")]

#Data Frames
pt_data<-data.frame(subject_Name, temperature, flu_status, gender, blood, symptons,stringsAsFactors = FALSE)
pt_data
pt_data$subject_Name
pt_data[c("temperature","flu_status")]
pt_data[1,2]
pt_data[c(1,3),c(2,4)]
pt_data[,1]
pt_data[1,]
pt_data[,]
pt_data[c(1,3),c("temperature","gender")]
pt_data[-2, c(-1,-3,-5,-6)]

#Matrix
m<-matrix(c(1,2,3,4),nrow=2)
m
m<-matrix(c(1,2,3,4),ncol=2)
m#Using column-major order
m<-matrix(c(1,2,3,4,5,6),nrow=2)
m
m<-matrix(c(1,2,3,4,5,6),ncol=2)
m
m[1,]
m[,1]

#Exploring and Understanding data
usedcars<-read.csv("usedcars.csv",stringsAsFactors = FALSE,header = TRUE)
str(usedcars)

#Exploring numeric variables
summary(usedcars$year)
summary(usedcars[c("price","mileage")])

#Measure the central tendency
mean(c(36000,44000,56000))
median(c(36000,44000,56000))

#Measure spread
range(usedcars$price)
diff(range(usedcars$price))
IQR(usedcars$price)
quantile(usedcars$price)
quantile(usedcars$price, probs=c(0.01,0.99))#Denoting cut points
quantile(usedcars$price, seq(from=0, to=1, by=0.2))#Obtain different slices of data

#Boxplot
boxplot(usedcars$price, main="Boxplot of Used Car Prices", ylab="Price ($)")
boxplot(usedcars$mileage, main="Boxplot of Used car Mileage", ylab="Odometer (mi.)")

#Histogram
hist(usedcars$price, main="Histogram of Used Car Prices", xlab="Price ($)")
hist(usedcars$mileage, main="Histogram of Used Car Mileage", xlab="Odometer (mi.)")

#Standard deviation and variance
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

#Exploring categorical variables
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)
model_table<-table(usedcars$model)
prop.table(model_table)#calculate proportions directly
color_table<-table(usedcars$color)
color_pct<-prop.table(color_table)*100
round(color_pct, digits=1)

#Scatterplot
plot(x=usedcars$mileage, y=usedcars$price,
     main="Scatterplot of Price vs. Mileage",
     xlab="Used Car Odometer",
     ylab="Used Car Price ($)")

#Cross-tabulations/crosstab/contingency table
library(gmodels)
usedcars$conservative<-usedcars$color %in% c("Black","Gray","Silver","White")
table(usedcars$conservative)
CrossTable(x=usedcars$model,y=usedcars$conservative)
