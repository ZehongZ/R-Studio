#Install packages
library(neuralnet)
#Input the library
mydata=read.csv("Squares.csv", sep=",", header = TRUE)
#Overview of the dataset
head(mydata)
attach(mydata)
names(mydata)
#Training the model based on output from input
model=neuralnet(formula=Output~Input, data = mydata, hidden=10, threshold = 0.01)
print(model)
#Plot the model
plot(model)
#Evaluate model performance
final_output=cbind(Input, Output, as.data.frame(model$net.result))
colnames(final_output)=c("Input", "Expected Output", "Neural Net Ouput")
print(final_output)

#Install package
library(nnet)
library(NeuralNetTools)
#Import datasets
mydata=read.csv('RestaurantTips.csv', sep=",", header = TRUE)
#Overview of the dataset
head(mydata)
attach(mydata)
names(mydata)
#Train the model
model=nnet(CustomerWillTip~Service+Ambience+Food,
           data=mydata,
           size=5,
           range=0.1,
           decay=5e-1,
           maxit=5000)
print(model)
plotnet(model)
garson(model)

