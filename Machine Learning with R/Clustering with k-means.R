#Import dataset
teens<-read.csv("snsdata.csv")

#Overview of the dataset
str(teens)

#Check the gender
table(teens$gender)

#Treat NA as a separate category
table(teens$gender, useNA = "ifany")

#Summary 
summary(teens$age)

#Clean out nonsense data
teens$age<-ifelse(teens$age>=13 & teens$age<20, teens$age, NA)
summary(teens$age)

#Transform NA as dummy variable
#If it's not female and NA, must be male
teens$female<-ifelse(teens$gender=="F"& !is.na(teens$gender),1,0)
teens$no_gender<-ifelse(is.na(teens$gender),1,0)
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

#Inputing the missing values
mean(teens$age)
mean(teens$age, na.rm = TRUE)
aggregate(data=teens, age~gradyear, mean, na.rm=TRUE)#It computes statistics for subgroups of data and avoids repeating
ave_age<-ave(teens$age, teens$gradyear, FUN=function(x) mean(x, na.rm=TRUE))
ave_age
#To impute these means ont othe missing values
teens$age<-ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)

#Training a model on the data
interests<-teens[5:40]
head(interests)

#Normalize the data
interests_z<-as.data.frame(lapply(interests, scale))

#Train a model
set.seed(2345)
teen_clusters<-kmeans(interests_z,5)

#Evaluating model performance
teen_clusters$size
teen_clusters$centers

#Improving model performance
teens$cluster<-teen_clusters$cluster
teens[1:5, c("cluster","gender","age","friends")]
aggregate(data=teens, age~cluster, mean)
aggregate(data=teens, female~cluster, mean)
aggregate(data=teens, friends~cluster, mean)
