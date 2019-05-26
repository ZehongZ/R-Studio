#Installing Keras
library(keras)
install_keras() #Installs the core keras library and TensorFlow

#IMDB dataset-Classification 
library(tensorflow)
install_tensorflow(version="nightly")
imdb<-dataset_imdb(num_words = 10000)
names(imdb)
c(c(train_data, train_labels),c(test_data, test_labels))%<-% imdb
str(train_data[1])
str(train_data[[1]])
train_labels[1]
max(sapply(train_data, max))

word_index<-dataset_imdb_word_index()#word_index is a named list mapping words to an integers index

reverse_word_index<-names(word_index)#Reverse it, mapping integer indices to words
names(reverse_word_index)<-word_index

#Encoding the integer sequences into a binary matrix
vectorize_sequences<-function(sequences, dimension=10000){
  results<-matrix(0, nrow=length(sequences),ncol=dimension)#Create an all-zero matrix of shape
  for (i in 1:length(sequences))
    results[i, sequences[[i]]]<-1
  results
}#Set specific indices of results[i] to 1s
x_train<-vectorize_sequences(train_data)
x_test<-vectorize_sequences(test_data)

#Convert your labels from integer to numeric
y_train<-as.numeric(train_labels)
y_test<-as.numeric(test_labels)

#Model definition
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

#Configuring the optimizer
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

history<-model%>%
  fit(
    partial_x_train,
    partial_y_train,
    epochs=20,
    batch_size=512,
    validation_data=list(x_val,y_val)
  )

plot(history)

#Training history with the plot() method
history_df<-as.data.frame(history)
str(history_df)

#Retraining a model from scratch
model<-keras_model_sequential()%>%
  layer_dense(units=16, activation = "relu", input_shape = c(10000))%>%
  layer_dense(units=16, activation = "relu")%>%
  layer_dense(units=1, activation = "sigmoid")

model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("accuracy")
)


model%>%fit(x_train, y_train, epochs=4, batch_size=512)
results<-model%>%evaluate(x_test, y_test)
results

#Using a trained network to generate predictions on new data
model%>%predict(x_test[1:10,])

#Boston Housing dataset-Regression 
library(keras)
dataset<-dataset_boston_housing()
c(c(train_data, train_targets),c(test_data, test_targets))%<-%dataset
str(train_data)
str(test_data)
#Normalizing the data
mean<-apply(train_data, 2, mean)
std<-apply(train_data,2,sd)
train_data<-scale(train_data, center = mean, scale = std)
test_data<-scale(test_data, center=mean, scale = std)
#Model definition 
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
}#Because you'll need to instantiate the same model multiple times, you use a function to construct it
#K-fold validation
k<-4
indices<-sample(1:nrow(train_data))
folds<-cut(indices, breaks=k, labels=FALSE)
num_epochs<-100
all_scores<-c()
for(i in 1:k){
  cat("processing fold #", i, "\n")
  val_indices<-which(folds==i, arr.ind=TRUE)
  val_data<-train_data[val_indices,]
  val_targets<-train_targets[val_indices]
  partial_train_data<-train_data[-val_indices,]
  partial_train_targets<-train_targets[-val_indices]
  model<-build_model()
  model%>%fit(partial_train_data, partial_train_targets,
              epochs=num_epochs, batch_size=1, verbose=0)
  results<-model%>%evaluate(val_data, val_targets, verbose=0)
  all_scores<-c(all_scores, results$mean_absolute_error)
}
all_scores
mean(all_scores)
#Saving the validation to logs at each fold
num_epochs<-500
all_mae_histories<-NULL
for(i in 1:k){
  cat("processing fold #",i, "\n")
  val_indices<-which(folds==i, arr.ind = TRUE)
  val_data<-train_data[val_indices,]
  val_targets<-train_targets[val_indices]
  partial_train_data<-train_data[-val_indices,]
  partial_train_targets<-train_targets[-val_indices]
  model<-build_model()
  history<-model%>%fit(
    partial_train_data,partial_train_targets,
    validation_data=list(val_data, val_targets),
    epochs=num_epochs, batch_size=1, verbose=0
  )
  mae_history<-history$metrics$val_mean_absolute_error
  all_mae_histories<-rbind(all_mae_histories,mae_history)
}
#Building the history of successive mean k-fold validation scores
average_mae_history<-data.frame(
  epoch=seq(1:ncol(all_mae_histories)),
  validation_mae=apply(all_mae_histories,2,mean)
)
#Plotting validation scores
library(ggplot2)
ggplot(average_mae_history,aes(x=epoch, y=validation_mae))+geom_line()
#Validation MAE by epoch:smoothed
ggplot(average_mae_history, aes(x=epoch,y=validation_mae))+geom_smooth()
#Training the final model
model<-build_model()
model%>%fit(train_data,train_targets,
            epochs=80, batch_size=16, verbose=0)
result<-model%>%evaluate(test_data, test_targets)
result#Off by about $2540
