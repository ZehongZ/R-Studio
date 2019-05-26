library(keras)
#Original network
model<-keras_model_sequential()%>%
  layer_dense(units=16, activation="relu",input_shape=c(10000))%>%
  layer_dense(units=16, activation ="relu")%>%
  layer_dense(units=1, activation = "sigmoid")
#Version of the model with lower capacity
model<-keras_model_sequential()%>%
  layer_dense(units=4, activation = "relu", input_shape = c(10000))%>%
  layer_dense(units=4, activation = "relu")%>%
  layer_dense(units=1, activation = "sigmoid")
#Version of the model with higher capacity
model<-keras_model_sequential()%>%
  layer_dense(units=512, activation = "relu", input_shape=c(10000))%>%
  layer_dense(units=512, activation = "relu")%>%
  layer_dense(units=1, activation = "sigmoid")

#Adding L2 weight regularization to the model
model<-keras_model_sequential()%>%
  layer_dense(units=16, kernel_regularizer = regularizer_l2(0.001),
              activation="relu", input_shape=c(10000))%>%
  layer_dense(units=16, kernel_regularizer = regularizer_l2(0.001),
              activation="relu")%>%
  layer_dense(units=1, activation = "sigmoid")
#regularizer_12(0.001) means every coefficient in the weight matrix of the layer will add 0.001*weight_coefficient_value to the total loss of the network

#Adding dropout to the IMDB network
model<-keras_model_sequential()%>%
  layer_dense(units=16, activation = "relu",input_shape=c(10000))%>%
  layer_dropout(rate=0.5)%>%
  layer_dense(units = 16, activation = "relu")%>%
  layer_dropout(rate=0.5)%>%
  layer_dense(units=1, activation = "sigmoid")
