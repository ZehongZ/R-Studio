####Understanding Neural Networks####
#Artificial Neural Network models the relationship between a set of input signals and an output signal using a model derived from our understanding of how a biological brain responds to stimuli from sensory inputs
#Just as a brain uses a network of interconnected cells called neurons to create a massive parallel processor, ANN uses a network of artificial neurons or nodes to solve learning problems
#Activation function: transforms a neuron's combined input signals into a single output signal to be broadcasted further in the network
#Network topology: describes the number of neurons in the model as well as the number of layers and manner in which they are connected
#Training algorithm that specifies how connection weights are set in order to inhibit or excite neurons in proportion to the input signal

####Activation Functions####
#The activation function is the mechanism by which the artificial neuron processes incoming information and passes it throughout the network
#Threshold activation function: an output signal only once a specified input threshold has been attained
#Sigmoid activation function:It's based on the natural logarithm. The output signal is no longer binary; output values can fall anywhere in the range from 0 to 1
#The sigmoid is differentiable which means that it is possible to calculate the derivative across the entire range of inputs
#Because this essentially squeezes the input values into a smaller range of outputs, activation functions like the sigmoid are sometimes called squashing functions
#The solution to the squashing problem is to transform all neural network inputs such that the features' values fall within a small range around 0. Typically this involves standardizing or normalizing the features.

####Network Topology####
#The ability of a neural network to learn is rooted in its topology, or the patterns and structures of interconnected neurons.
#The key characteristics: The number of layers, Whether informatino in the network is allowed to travel backward, the number of nodes within each layer of the network
#Generally, larger and more complex networks are capable of identifying more subtle patterns and complex decision boundaries

####The number of layers####
#Input nodes receives unprocessed signals directly from the input data
#Each input node is responsible for processeing a single feature in the dataset
#The feature's value will be transformed by the corresponding node's activation function
#The signals sent by the input nodes are received by the output node, which uses its own activation function to generate a final prediction
#The input and output nodes are arranged in groups known as layers
#Multilayer network adds one or more hidden layers that process the signals from the input nodes prior to it reaching the output nodes

####The direction of information travel####
#Network in which the input signal is fed continuously in one direction from connection to connection until it reaches the output layer as called feedforward networks
#A neural network with multiple hidden layers is called a Deep Neural Network and the proactive of training such network is sometimes referrred to as deep learning
#A recurrent network or feedback network allows signals to travel in both directions using loops. The addition of a short-term memeory, or delay, increases the power of recurrent network immensely.

####The number of nodes in each layer####
#The number of input nodes is predetermined by the number of features in the input data
#The number of input nodes is predetermined by the number of outcomes to be modeled or the number of class levels in the outcome
#The number of hidden nodes is left to the user to decide prior to training the model

####Training neural networks with backpropagation####
#The backpropgation algorithm iterates through many cycles of two process. Each cylce is known as an epoch
#Each epoch in the backpropagation algorithm includes a forward phase and a backward phase
#Forward phase: in which the neurons are activated in sequence from the input layer to the output layer, applying each neuron's weights and activation function along the way.
#Backward phase: in which the network's output signal resulting from the forward phase is compared to the true target value in the training data. The difference between the network's output signal and the true value results in an error that is propagated backwards in the network to modify the connection weights between neurons and reduce future errors
#Gradient descent technique determine algorithm how much a weight should be changed
#The gradient suggets how steeply the error will be reduced or increased for a change in the weight.The algorithm will attempt to change the weights that result in the greates reduction in error by an amount known as the learning rate.
#The greater the learning rate, the faster the algorithm will attempt to descend down the gradients, which could reduce the training time at the risk of overshotting the valley

####Modeling the strength of concrete with ANNs####
concrete<-read.csv("concrete_data.csv",header = T)
str(concrete)
names(concrete)
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}#Cause Neural networks work best when the input data are scaled to a narrow range around zero
concrete_norm<-as.data.frame(lapply(concrete,normalize))
head(concrete_norm)
summary(concrete_norm$concrete_compressive_strength)
dim(concrete_norm)
#Create training and testing set
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]
#Train a model
library(neuralnet)
concrete_model<-neuralnet(concrete_compressive_strength~blast_furnace_slag+fly_ash+water+superplasticizer+coarse_aggregate+fine_aggregate+age, data=concrete_train)
plot(concrete_model)
#Evaluating model performance
model_results<-compute(concrete_model, concrete_test[1:8])
predict_strength<-model_results$net.result
predict_strength
cor(predict_strength, concrete_test$concrete_compressive_strength)
#Improve model performance
concrete_model2<-neuralnet(concrete_compressive_strength~blast_furnace_slag+fly_ash+water+superplasticizer+coarse_aggregate+fine_aggregate+age, data=concrete_train, hidden = 5)
plot(concrete_model2)
model_result2<-compute(concrete_model2, concrete_test[1:8])
predicted_strength2<-model_result2$net.result
cor(predicted_strength2, concrete_test$concrete_compressive_strength)
