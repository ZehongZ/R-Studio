#Neural Network regression with the Boston Dataset
#Install packages
library(MASS)
library(neuralnet)
#Import datasets
set.seed(1)
data("Boston")
str(Boston)
#Train model
max_data<-apply(Boston, 2, max)
min_data<-apply(Boston, 2, min)
data_scaled<-scale(Boston, center=min_data, scale = max_data-min_data)
index=sample(1:nrow(Boston), round(0.7*nrow(Boston)))
train_data<-as.data.frame(data_scaled[index,])
test_data<-as.data.frame(data_scaled[-index,])
n=names(Boston)
f=as.formula(paste("medv~", paste(n[!n %in% "medv"], collapse = "+")))
net_data=neuralnet(f, data=train_data, hidden = 10, linear.output = T)
#Plot the model
plot(net_data)
#Model Predictions
predict_net_test_start <- predict_net_test$net.result*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)
test_start <- as.data.frame((test_data$medv)*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv))
MSE.net_data <- sum((test_start - predict_net_test_start)^2)/nrow(test_start)
#Regression Model
regression_model<-lm(medv~., data=Boston)
summary(regression_model)
test<-Boston[-index,]
predict_lm<-predict(regression_model, test)
MSE.lm<-sum((predict_lm-test$medv)^2)/nrow(test)
MSE.net_data
MSE.lm

#Neural Networks using Kohonen package
library(kohonen)
data("wines")
str(wines)
head(wines)
View(wines)
set.seed(1)
som.wines=som(scale(wines), grid=somgrid(5,5,"hexagonal"))#Create a 5*5 matrix
som.wines
dim(getCodes(som.wines))
plot(som.wines, main="Wine Data Kohonen SOM")#Clusters
par(mfrow=c(1,1))
plot(som.wines, type="changes", main="Wine data:SOM")#Plot the mean distance to the closest unit vs the number of iterations
#Train model
training=sample(nrow(wines),150)
xtrainin=scale(wines[training,])
xtest=scale(wines[-training,],
            center=attr(xtrainin, "scaled:center"),
            scale=attr(xtrainin, "scaled:scale"))
trainingdata=list(measurements=xtrainin,
                  vintages=vintages[training])
testdata=list(measurements=xtest, vintages=vintages[-training])
mygrid=somgrid(5,5,"hexagonal")
som.wines=supersom(trainingdata, grid=mygrid)
som.prediction=predict(som.wines, newdata=testdata)
table(vintages[-training], som.prediction$predictions[["vintages"]])
