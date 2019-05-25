####Install Keras####
library(keras)
install_keras()

####Developing with Keras####
#Define your training data:input tensors and target tensors
#Define a network layers that maps your inputs to your targets
#Configure the learning process by choosing a loss function, an optimizier, and some metrics to monitor
#Iterate on your training data by calling the fit() method of your model

#Two ways to define a model: keras_model_sequential() for liner stacks of layers
#functional APT for directred acyclie graphs of layers, which let you build compltely arbitrary architectures

####keras models sequential####
model<-keras_model_sequential()%>%
  layer_dense(units=32, input_shape = c(784))%>%
  layer_dense(units=10, activation = "softmax")

####function API####
input_tensor<-layer_input(shape=c(784))
output_tensor<-input_tensor%>%
  layer_dense(units=32, activation = "relu")%>%
  layer_dense(units=10, activation = "softmax")
model<-keras_model(inputs=input_tensor, outputs=output_tensor)

####single loss function####
model%>% compile(
  optimizer=optimizer_rmsprop(lr=0.0001),
  loss="mse",
  metric=c("accuracy")
)
#Parsing arrays of input data to the model via the fit() method
model%>%fit(input_tensor, target_tensor, batch_size=128, epochs=10)

####Setting up a Deep-Learning Workstation
#It's highly recommended that you run deep-learning code on a modern NVIDIA GPU.
#If you don't want to install a GPU on your machine, you can alternatively consider running your experiments on an AWS EC2 GPU instance or on Google Cloud Platform

####Classifying Movie Review: A binary classification Example####
devtools::install_github("rstudio/reticulate")
library(tensorflow)
install_tensorflow(version="nightly")
library(keras)
imdb<-dataset_imdb(num_words = 10000)#Top 10,000 most frequently occuring words
c(c(train_data, train_labels),c(test_data, test_labels))%<-%imdb
train_data<-imdb$train$x
train_labels<-imdb$train$y
test_data<-imdb$test$x
test_labels<-imdb$test$y
str(train_data[1])
train_labels[[1]]
max(sapply(train_data,max))
#Decode one of these reviews
word_index<-dataset_imdb_word_index()
reverse_word_index<-names(word_index)
names(reverse_word_index)<-word_index
decode_review<-sapply(train_data[[1]], function(index){
  word<-if (index>=3) reverse_word_index[[as.character(index-3)]]
  if (!is.null(word)) word else "?"
})

####Preparing the data####
#Have to turn your lists into tensors
#Pad your lists so that they all have the same length, turn them into an integer tensor of shape
#One-hot encode your lists to turn them into vectors of os and ls
#Vectorize the data
vectorize_sequences<-function(sequences, dimension=10000){
  results<-matrix(0, nrow=length(sequences), ncol=dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]]<1
  results
}
x_train<-vectorize_sequences(train_data)
x_test<-vectorize_sequences(test_data)
str(x_train[1,])
#Convert your labels from integer to numeric
y_train<-as.numeric(train_labels)
y_test<-as.numeric(test_labels)

####Building your network####
library(keras)
model<-keras_model_sequential()%>%
  layer_dense(units=16, activation = "relu", input_shape=c(10000))%>%
  layer_dense(units=16, activation = "relu")%>%
  layer_dense(units=1, activation = "sigmoid")
#Compiling the model
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("accuracy")
)
#Configure the optimizer
model%>%compile(
  optimizer=optimizer_rmsprop(lr=0.001),
  loss="binary_crossentropy",
  metrics=c("accuracy")
)
#Using custom losses and metrics
model%>%compile(
  optimizer=optimizer_rmsprop(lr=0.001),
  loss=loss_binary_crossentropy,
  metrics=metric_binary_accuracy
)
####Validating your approach####
#Setting aside a validation set
val_indices<-1:10000
x_val<-x_train[val_indices,]
partial_x_train<-x_train[-val_indices,]
y_val<-y_train[val_indices]
partial_y_train<-y_train[-val_indices]
#Training your model
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("accuracy")
)
history<-model%>%fit(
  partial_x_train,
  partial_y_train,
  epochs=20,
  hatch_size=512,
  validation_data=list(x_val,y_val)
)
str(history)
plot(history)

####Training history with the plot() method####
history_df<-as.data.frame(history)
str(history_df)
#Retraining a model from scratch
model<-keras_model_sequential()%>%
  layer_dense(units=16, activation = "relu",input_shape=c(10000))%>%
  layer_dense(units=16, activation = "relu")%>%
  layer_dense(units=1, activation = "sigmoid")
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("accuracy")
)
model%>%fit(x_train, y_train, epochs=4, batch_size=512)
results<-model%>%evaluate(x_test, y_test)
results#Accuracy = 50%

####Using a trained network to generate predictions on new data
model%>%predict(x_test[1:10,])

####Predicting House Prices####
library(keras)
dataset<-dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets))%<-% dataset
str(train_data)#404 training sames, 13 numeriacal feature
str(test_data)#102 test samples, 13 numerical features
str(train_targets)
#Preparing the data
mean<-apply(train_data, 2, mean)
std<-apply(train_data, 2, sd)
train_data<-scale(train_data, center=mean, scale=std)
test_data<-scale(test_data, center=mean, scale=std)
#Building network
build_model<-function(){
  model<-keras_model_sequential()%>%
    layer_dense(units=64, activation = "relu",
                input_shape = dim(train_data)[[2]])%>%
    layer_dense(units=64, activation = "relu")%>%
    layer_dense(units=1)
  model%>%compile(
    optimizer="rmsprop",
    loss="mse",
    metrics=c("mae")
  )
}#Becaues you'll need to instantiate the same model multiple times, you use a function to construct it
#Validating approach using k-fold validation
k<-4
indices<-sample(1:nrow(train_data))
folds<-cut(indices, breaks = k, labels = FALSE)
num_epochs<-100
all_scores<-c()
for (i in 1:k){
  cat("processing fold #", i, "\n")
  val_indices<-which(folds==i, arr.ind=TRUE)
  val_data<-train_data[val_indices,]
  val_targets<-train_targets[val_indices]
  partial_train_data<-train_data[-val_indices,]
  partial_train_targets<-train_targets[-val_indices]
  model<-build_model()
  model%>%fit(partial_train_data, partial_train_targets,
              epochs=num_epochs, batch_size=1, verbose=0)
  result<-model%>%evaluate(val_data, val_targets, verbose=0)
  all_scores<-c(all_scores,results$mean_absolute_error)
}
all_scores
mean(all_scores)
