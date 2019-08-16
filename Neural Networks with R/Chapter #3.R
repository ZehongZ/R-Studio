#Deep learning with neuralnet
library(neuralnet)
library(ISLR)
#Import dataset
data=College
#Overview of the dataset
str(data)
head(data)
#Transform dataset
max_data<-apply(data[,2:18],2,max)
min_data<-apply(data[,2:18],2,min)
data_scaled<-scale(data[,2:18], center=min_data, scale=max_data-min_data)
Private=as.numeric(College$Private)-1
data_scaled=cbind(Private, data_scaled)
head(data)
head(data_scaled)
#Split the data into training and test data
index=sample(1:nrow(data), round(0.7*nrow(data)))
train_data<-as.data.frame(data_scaled[index,])
test_data<-as.data.frame(data_scaled[-index,])
#Train the model
n=names(train_data)
f<-as.formula(paste("Private~", paste(n[!n %in% "Private"], collapse = " + ")))
deep_net=neuralnet(f, data=train_data, hidden = c(5,3), linear.output = F)#Two hidden levels with respective five neurons in the first hidden layer and three neurons in the second
plot(deep_net)
#Model Evaluation
predicted_data<-compute(deep_net, test_data[,2:18])
print(head(predicted_data$net.result))
predicted_data$net.result<-sapply(predicted_data$net.result, round, digits=0)
table(test_data$Private, predicted_data$net.result)

#Training and Modeling a DNN using H2O
#Import library
library(h2o)
c1=h2o.init(max_mem_size = "2G",
            nthreads=2,
            ip="localhost",
            port=54321)
#Import dataset
data("iris")
#Overview dataset
summary(iris)
#Data transformation
iris_d1<-h2o.deeplearning(1:4,5,as.h2o(iris), hidden = c(5,5),
                          export_weights_and_biases = T)
iris_d1
plot(iris_d1)
h2o.weights(iris_d1, matrix_id = 1)
h2o.weights(iris_d1, matrix_id = 2)
h2o.weights(iris_d1, matrix_id = 3)
h2o.biases(iris_d1, vector_id = 1)
h2o.biases(iris_d1, vector_id = 2)
h2o.biases(iris_d1, vector_id = 3)
#Plot weights connecting 'Sepal.Length' to first hidden neurons
plot(as.data.frame(h2o.weights(iris_d1, matrix_id = 1))[,1])
h2o.confusionMatrix(iris_d1)
pairs(iris[1:4], main="Scatterplot matrices of Iris Data", pch=21, bg=F)
#Evaluate the accuracy
h2o.hit_ratio_table(iris_d1)
h2o.r2(iris_d1)
#Fit generalized linear models
m=iris.lm<-h2o.glm(x=2:5, y=1, training_frame = as.h2o(iris))
#Calculate the model's coefficient of determination
h2o.r2(m)

#Deep autoencoders using H2)
anomaly_model<-h2o.deeplearning(1:4,
                                training_frame = as.h2o(iris),
                                activation = "Tanh",
                                autoencoder = TRUE,
                                hidden = c(50,20,50),
                                sparse = TRUE,
                                l1=1e-4,
                                epochs = 100)
