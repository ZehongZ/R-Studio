#Nearest neighbor classifiers are defined by their characteristic of classifying unlabeled examples by assigning them the class of similar labeled examples
#Sensitive to noise
#If a concept is difficult to define, but you know it when you see it, then nearest neighbors might be appropriate

####Measuring similarity with distance####
#Using Euclidean distance, which is the distance one would measure if it were possible to use a ruler to connect two points

####Preparing data for use with k-NN####
#If certain features have a much larger range of values than the others, the distance measurements will be strongly dominated by the features with larger ranges
#The tradional method of rescaling features for k-NN is min-max normalization
#The process transforms a feature such that all of its values fall in a range between 0 and 1
#Another common transformation is called is z-score standardization
#The z-score fall in an unbound range of negative and postive numbers

####Diagonosing breast cancer with the k-NN algorithm
myd=read.csv("data.csv",header = T)
myd=myd[-1]
table(myd$diagnosis)
myd$diagnosis<-factor(myd$diagnosis, levels = c("B","M"),labels = c("Benign","Malignant"))
round(prop.table(table(myd$diagnosis))*100, digits=1)
summary(myd[c("radius_mean","area_mean","smoothness_mean")])
#Transformation using Min-Max transformation
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
myd_n<-as.data.frame(lapply(myd[2:31],normalize))
myd_z<-as.data.frame(scale(myd[-1]))#z-score stanadization
summary(myd_n$area_mean)
#Create training and testing dataset
myd_train<-myd_n[1:469,]
myd_test<-myd_n[470:569,]
#Store these class labels in factor vectors
myd_train_labels<-myd[1:469,1]
myd_test_labels<-myd[470:569,1]
#Training a model
library(class)
myd_pred<-knn(train = myd_train, test=myd_test,cl=myd_train_labels,k=21)
CrossTable(x=myd_test_labels,y=myd_pred,prop.chisq = FALSE)
#Top-left cell=true negative
#Bottom-right cell=true positive
#Lower-left cell=false negative(model predict Benign, but actually malignant; Very Dangerous)
#Top-right cell=false positive
