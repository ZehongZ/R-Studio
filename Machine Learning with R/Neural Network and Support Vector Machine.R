#Import dataset
concrete<-read.csv("concrete.csv",header=T)

#Overview of the dataset
str(concrete)

#Create min-max function
normalize<-function(x){
  return ((x-min(x))/ (max(x)-min(x)))
}

#Normalize concrete data
concrete_norm<-as.data.frame(lapply(concrete, normalize))
summary(concrete_norm)
summary(concrete_norm$strength)
summary(concrete$strength)

#Create training and test datasets
concrete_train<-concrete_norm[1:773,]
concret_test<-concrete_norm[774:1030,]

#Training a model on the data
library(neuralnet)
concrete_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data=concrete_train)

#Visualize the topology
plot(concrete_model)

#Evaluate model performance
model_results<-compute(concrete_model, concret_test[1:8])
predicted_strength<-model_results$net.result
cor(predicted_strength, concret_test$strength)

#Improving model performance
concrete_model12<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data=concrete_train, hidden = 5)

#Visualize the model
plot(concrete_model12)

#Evaluate performance
model_result2<-compute(concrete_model12, concret_test[1:8])
predicted_strength2<-model_result2$net.result
cor(predicted_strength2, concret_test$strength)
