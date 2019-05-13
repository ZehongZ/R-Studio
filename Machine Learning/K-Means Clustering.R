####Understanding Clustering####
#Clustering is an unsupervised machine learning task that automatically divides the data into clusters, or groups of similar items. 
#clustering is used for knowledge discovery rather than prediction

####The K-means clustering algorithm####
#Disadvantage: Not ideal for non-spherical clusters or clusters of widely varying density
#The algorithm uses heuristic process that find locally optimal solutions
#It starts with an initial guess for the cluster assignments, and then modifies the assignments slightly to see whether the changes improve the homogeneity within the clusters

####Using distance to assign and update clusters####
#K-means uses Euclidean distance, but Manhattan distance or Minkowski distance are also sometimes used
#Centroid: it is calculated as the average position of the points currently assigned to that cluster

####Finding teen market segements using k-means clustering
teens<-read.csv("snsdata.csv")
str(teens)
table(teens$gender)
table(teens$gender, useNA = "ifany")#Including NA
summary(teens$age)
teens$age<-ifelse(teens$age>=13&teens$age<20, teens$age, NA)
summary(teens$age)
teens$female<-ifelse(teens$gender=="F" &
                        !is.na(teens$gender),1,0)
teens$no_gender<-ifelse(is.na(teens$gender),1,0)
table(teens$gender,useNA = "ifany")
table(teens$female,useNA = "ifany")
table(teens$no_gender,useNA = "ifany")
#Inputing the missing values
mean(teens$age)
mean(teens$age, na.rm=TRUE)
aggregate(data=teens, age~gradyear, mean, na.rm=TRUE)
ave_age<-ave(teens$age, teens$gradyear, FUN=function(x) mean(x,na.rm=TRUE))
teens$age<-ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)
#Training a model on the data
library(stats)
interests<-teens[5:40]
interests_z<-as.data.frame(lapply(interests, scale))
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
aggregate(data=teens, friends~cluster,mean)
