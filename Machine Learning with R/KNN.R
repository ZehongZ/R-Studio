#Import the dataset
wbcd<-read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

#Overview of the dataset
str(wbcd)

#Drop the ID column
wbcd<-wbcd[-1]
names(wbcd)

#Explore the class variable
table(wbcd$diagnosis)

#Transform the target variable as a factor
wbcd$diagnosis<-factor(wbcd$diagnosis, levels = c("B","M"), labels=c("Benign","Malignant"))

#Check out the output
table(wbcd$diagnosis)
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)

#Check the numeric variables
summary(wbcd$radius_mean,wbcd$area_mean,wbcd$smoothness_mean)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

#Creating a normalize() function
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

#Testing the function
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

#lapply() function takes a list and applies a specified function to each list element
wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize))
summary(wbcd_n$area_mean)
wbcd_n2<-normalize(wbcd[2:31])
summary(wbcd_n2$area_mean)

#Create training and test datasets
wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]

#Create target variables
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

#Training a model
library(class)
wbcd_test_pred<-knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels,k=21)

#Evaluating model performance
library(gmodels)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred, prop.chisq = FALSE)

#Improving Performance
wbcd_z<-as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
wbcd_train<-wbcd_z[1:469,]
wbcd_test<-wbcd_z[470:569,]
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]
wbcd_test_pred<-knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=21)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq = FALSE)
