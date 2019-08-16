#Build, train and test neural networks
#Install libraries
library(neuralnet)
library(ISLR)
#Import data
data("Auto")
data=Auto
#Plot weight 
plot(data$weight, data$mpg,pch=data$origin,cex=2)
#Plot the dataset in 2*2
par(mfrow=c(2,2))
plot(data$cylinders, data$mpg, pch=data$origin, cex=1)
plot(data$displacement,data$mpg,pch=data$origin,cex=1)
plot(data$horsepower,data$mpg,pch=data$origin,cex=1)
plot(data$acceleration,data$mpg,pch=data$origin,cex=1)
#Calculate mean
mean_data<-apply(data[1:6],2,mean)
#Calculate standardeviation
sd_data<-apply(data[1:6],2,sd)
#Normalize the dataset
data_scaled<-as.data.frame(scale(data[1:6],center=mean_data, scale = sd_data))
head(data_scaled)
#Split the data into training and test
index=sample(1:nrow(data), round(0.7*nrow(data)))
train_data=as.data.frame(data_scaled[index,])
test_data=as.data.frame(data_scaled[-index,])
#Build the formula
n=names(data_scaled)
f=as.formula(paste("mpg~", paste(n[!n%in%"mpg"], collapse = "+")))
f
#Train the model
net=neuralnet(f, data=train_data,hidden = 3,linear.output = TRUE)
summary(net)
net
#Visualize the data
plot(net)
#Prediction
predict_net_test<-compute(net,test_data[,2:6])
mse.net<-sum((test_data$mpg-predict_net_test$net.result)^2)/nrow(test_data)
#Train linear model
lm_mod<-lm(mpg~.,data=train_data)
summary(lm_mod)
#Model evaluation
predict_lm<-predict(lm_mod,test_data)
mse_lm<-sum((predict_lm-test_data$mpg)^2)/nrow(test_data)
#Compare model
par(mfrow=c(1,2))
plot(test_data$mpg, predict_net_test$net.result, col="black",main="Real vs. Prediction")
abline(0,1,lwd=5)
plot(test_data$mpg, predict_lm, col="black",main = "Real vs. Predicted for linear")
abline(0,1,lwd=5)

#Classifying Brest Cancer
#Install packages
library(mlbench)
library(neuralnet)
#Import data
data("BreastCancer")
#Summary of the dataset
summary(BreastCancer)
#Clean up missing data
mvindex=unique(unlist(lapply(BreastCancer, function (x) which (is.na(x)))))
data_cleaned<-na.omit(BreastCancer)
#Summary of clean dataset 
summary(data_cleaned)
#Explore numeric datasets
boxplot(data_cleaned[,2:10])
hist(as.numeric(data_cleaned$Mitoses))
par(mfrow=c(3,3))
hist(as.numeric(data_cleaned$Cl.thickness))
hist(as.numeric(data_cleaned$Cell.size))
hist(as.numeric(data_cleaned$Cell.shape))
hist(as.numeric(data_cleaned$Marg.adhesion))
hist(as.numeric(data_cleaned$Epith.c.size))
hist(as.numeric(data_cleaned$Bare.nuclei))
hist(as.numeric(data_cleaned$Bl.cromatin))
hist(as.numeric(data_cleaned$Normal.nucleoli))
hist(as.numeric(data_cleaned$Mitoses))
#Transform data
str(data_cleaned)
input<-data_cleaned[,2:10]
indx<-sapply(input, is.factor)
input<-as.data.frame(lapply(input, function(x) as.numeric(as.character(x))))
max_data<-apply(input,2,max)
min_data<-apply(input,2,min)
input_scaled<-as.data.frame(scale(input, center=min_data,scale = max_data))
View(input_scaled)
#Class variable
cancer<-data_cleaned$Class
cancer<-as.data.frame(cancer)
cancer<-with(cancer, data.frame(model.matrix(~cancer+0)))
final_data<-as.data.frame(cbind(input_scaled,cancer))
#Split training and train data
index=sample(1:nrow(final_data),round(0.7*nrow(final_data)))
train_data<-as.data.frame(final_data[index,])
test_data<-as.data.frame(final_data[-index,])
#Build model
n=names(final_data[1:9])
f=as.formula(paste("cancerbenign+cancermalignant~", paste(n, collapse = " + ")))
net=neuralnet(f, data=train_data, hidden = 5, linear.output = FALSE)
plot(net)
#Model evaluation
predict_net_test<-compute(net, test_data[,1:9])
predict_result<-round(predict_net_test$net.result,digits=0)
net.prediction=c("benign","malignant")[apply(predict_result,1, which.max)]
predict.table=table(data_cleaned$Class[-index],net.prediction,prop.table=FALSE)
library(gmodels)
CrossTable(x=data_cleaned$Class[-index],y=net.prediction,prop.chisq = FALSE)
