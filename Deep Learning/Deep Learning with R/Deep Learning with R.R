##Classifying Movie Reviews: A Binary Classification##
#Import packages
library(keras)
#Loading dataset
imdb<-dataset_imdb(num_words = 10000)
c(c(train_data, train_labels),c(test_data, test_labels))%<-%imdb
str(train_data)
str(train_data[1])
str(train_data[[1]])
train_labels[1]
train_labels[[1]]
#See the max index
max(sapply(train_data,max))
#Decode reviews back to English words
word_index<-dataset_imdb_word_index()
head(word_index)
reverse_word_index<-names(word_index)
head(reverse_word_index)
names(reverse_word_index)<-word_index
head(word_index)
decoded_review<-sapply(train_data[[1]], function(index){
  word<-if (index>=3) reverse_word_index[[as.character(index-3)]]
  if (!is.null(word)) word else"?"
})
#Encode the integer sequences into a binary matrix
victorize_sequences<-function(sequences, dimension=10000){
  results<-matrix(0, nrow=length(sequences), ncol=dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]]<-1
  results
}
x_train<-victorize_sequences(train_data)
x_test<-victorize_sequences(test_data)
str(x_train[1,])
#Convert labels from integer to numeric
y_train<-as.numeric(train_labels)
y_test<-as.numeric(test_labels)
#Model Definition
model<-keras_model_sequential()%>%
  layer_dense(units = 16, activation = "relu", input_shape = c(10000))%>%
  layer_dense(units = 16, activation = "relu")%>%
  layer_dense(units = 1, activation = "sigmoid")
#Compling the Model
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("accuracy")
)
#Using Custom Losses and Metrics
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
#Train model
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("accuracy")
)
history<-model%>%fit(
  partial_x_train,
  partial_y_train,
  epochs=20,
  batch_size=512,
  validation_data=list(x_val, y_val)
)
plot(history)
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
#Predict on new data
model%>%predict(x_test[1:10,])


##Classifying Newswire: A Multiclass Classification##
#Import library
library(keras)
#Import dataset
reuters<-dataset_reuters(num_words = 10000)
c(c(train_data, train_labels),c(test_data,test_labels))%<-%reuters
length(train_data)
length(test_data)
#Decoding newswires back to text
word_index<-dataset_reuters_word_index()
reverse_word_index<-names(word_index)
names(reverse_word_index)<-word_index
decoded_newswire<-sapply(train_data[[1]], function(index){
  word<-if (index>=3) reverse_word_index[[as.character(index-3)]]
  if (!is.null(word)) word else "?"
})
#Encoding the data
vectorize_sequences<-function(sequences, dimension=10000){
  results<-matrix(0, nrow=length(sequences),ncol=dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]]<-1
  results
}
x_train<-vectorize_sequences(train_data)
x_test<-vectorize_sequences(test_data)
#one-hot encoding
to_one_hot<-function(labels, dimension=46){
  results<-matrix(0, nrow=length(labels), ncol=dimension)
  for (i in 1:length(labels))
    results[i, labels[[i]]+1]<-1
  results
}
one_hot_train_labels<-to_one_hot(train_labels)
one_hot_test_labels<-to_one_hot(test_labels)
one_hot_train_labels<-to_categorical(train_labels)
one_hot_text_labels<-to_categorical(test_labels)
#Building your network
model<-keras_model_sequential()%>%
  layer_dense(units=64, activation = "relu", input_shape = c(10000))%>%
  layer_dense(units=64, activation = "relu")%>%
  layer_dense(units=46, activation = "softmax")
#Compiling the model
model%>%compile(
  optimizer="rmsprop",
  loss="categorical_crossentropy",
  metrics=c("accuracy")
)
#Setting aside a validation set
val_indices<-1:1000
x_val<-x_train[val_indices,]
partial_x_train<-x_train[-val_indices,]
y_val<-one_hot_train_labels[val_indices,]
partial_y_train=one_hot_train_labels[-val_indices,]
#Training the model
history<-model%>%fit(
  partial_x_train,
  partial_y_train,
  epochs=20,
  batch_size=512,
  validation_data=list(x_val, y_val)
)
plot(history)
#Retraiing a model from scratch
model<-keras_model_sequential()%>%
  layer_dense(units=64, activation = "relu", input_shape=c(10000))%>%
  layer_dense(units=64, activation = "relu")%>%
  layer_dense(units=46, activation = "softmax")
model%>%compile(
  optimizer="rmsprop",
  loss="categorical_crossentropy",
  metrics=c("accuracy")
)
history<-model%>%fit(
  partial_x_train,
  partial_y_train,
  epochs=9,
  batch_size=512,
  validation_data=list(x_val, y_val)
)
results<-model%>%evaluate(x_test, one_hot_test_labels)
results
#Compare to random baseline
test_labels_copy<-test_labels
test_labels_copy<-sample(test_labels_copy)
length(which(test_labels==test_labels_copy))/length(test_labels)
#Generating predictions for new data
predictions<-model%>%predict(x_test)
dim(predictions)
sum(predictions[1,])
which.max(predictions[1,])


##Predicting House Prices: A regression##
#Loading the Boston Housing Dataset
library(keras)
dataset<-dataset_boston_housing()
c(c(train_data, train_targets),c(test_data, test_targets))%<-%dataset
str(train_data)
str(test_data)
str(train_targets)
#Normalizing the data
mean<-apply(train_data, 2, mean)
std<-apply(train_data, 2, sd)
train_data<-scale(train_data, center=mean, scale=std)
test_data<-scale(test_data, center=mean, scale=std)
#Model definition
build_model<-function(){
  model<-keras_model_sequential()%>%
    layer_dense(units=64, activation = "relu", input_shape = dim(train_data)[[2]])%>%
    layer_dense(units=64, activation = "relu")%>%
    layer_dense(units=1)
  model%>%compile(
    optimizer="rmsprop",
    loss="mse",
    metrics=c("mae")
  )
}
#K-fold Validation
k<-4
indices<-sample(1:nrow(train_data))
folds<-cut(indices, breaks=k, labels=FALSE)
num_epochs<-100
all_scores<-c()
for(i in 1:k){
  cat("processing fold #",i,"\n")
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
#Saving the validation logs at each fold
num_epochs<-500
all_mae_histories<-NULL
for (i in 1:k){
  cat("processing fold #", i, "\n")
  val_indices<-which(folds==i, arr.ind=TRUE)
  val_data<-train_data[val_indices,]
  val_targets<-train_targets[val_indices]
  partial_train_data<-train_data[-val_indices,]
  partial_train_targets<-train_targets[-val_indices]
  model<-build_model()
  history<-model%>%fit(
    partial_train_data, partial_train_targets,
    validation_data=list(val_data, val_targets),
    epochs=num_epochs,batch_size=1, verbose=0
  )
}
mae_history<-history$metrics$val_mean_absolute_error
all_mae_histories<-rbind(all_mae_histories,mae_history)
#Building the history of successive mean k-fold validation scores
average_mae_history<-data.frame(
  epoch=seq(1:ncol(all_mae_histories)),
  validation_mae=apply(all_mae_histories,2,mean)
)
#Plotting validation scores
library(ggplot2)
ggplot(average_mae_history, aes(x=epoch, y=validation_mae))+geom_line()
#Training the final model
model<-build_model()
model%>%fit(train_data, train_targets,
            epochs=80, batch_size=16, verbose=0)
result<-model%>%evaluate(test_data, test_targets)

##Introduce a small convet##
library(keras)
#Instantiating a small convnet
model<-keras_model_sequential()%>%
  layer_conv_2d(filters=32, kernel_size = c(3,3),activation = "relu", input_shape=c(28,28,1))%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters=64, kernel_size=c(3,3), activation="relu")
#Adding a classifie on top of the convnet
model<-model%>%
  layer_flatten()%>%
  layer_dense(units=64, activation = "relu")%>%
  layer_dense(units=10, activation="softmax")
#Training the convnet on MNIST images
mnist<-dataset_mnist()
c(c(train_images, train_labels), c(test_images, test_labels))%<-%mnist
train_images<-array_reshape(train_images,c(60000,28,28,1))
train_images<-train_images/255
test_images<-array_reshape(test_images, c(10000,28,28,1))
test_images<-test_images/255
train_labels<-to_categorical(train_labels)
test_labels<-to_categorical(test_labels)
model%>%compile(
  optimizer="rmsprop",
  loss="categorical_crossentropy",
  metrics=c("accuracy")
)
model%>%fit(
  train_images, train_labels,
  epochs=5, batch_size=64
)
#Evaluate the model on the test data
results<-model%>%evaluate(test_images, test_labels)
results


##Deep Learning Using Cats and Dogs##
#Copying images to training, validation and test directories
original_dataset_dir<-"~/Desktop/Database/dogs vs cats/train"
base_dir<-"~/Desktop/Database/dogs vs cats/cats_and_dogs_small"
dir.create(base_dir)
train_dir<-file.path(base_dir, "train")
dir.create(train_dir)
validation_dir<-file.path(base_dir, "validation")
dir.create(validation_dir)
test_dir<-file.path(base_dir,"test")
dir.create(test_dir)
train_cats_dir<-file.path(train_dir,"cats")
dir.create(train_cats_dir)
train_dogs_dir<-file.path(train_dir, "dogs")
dir.create(train_dogs_dir)
validation_cats_dir<-file.path(validation_dir, "cats")
dir.create(validation_cats_dir)
validation_dogs_dir<-file.path(validation_dir,"dogs")
dir.create(validation_dogs_dir)
test_cats_dir<-file.path(test_dir,"cats")
dir.create(test_cats_dir)
test_dogs_dir<-file.path(test_dir, "dogs")
dir.create(test_dogs_dir)
fnames<-paste0("cat.",1:1000,".jpg")
file.copy(file.path(original_dataset_dir,fnames),file.path(train_cats_dir))
fnames<-paste0("cat.",1001:1500,".jpg")
file.copy(file.path(original_dataset_dir,fnames),file.path(validation_cats_dir))
fnames<-paste0("cat.",1501:2000,".jpg")
file.copy(file.path(original_dataset_dir,fnames),file.path(test_cats_dir))
fnames<-paste0("dog.",1:1000,".jpg")
file.copy(file.path(original_dataset_dir,fnames),file.path(train_dogs_dir))
fnames<-paste0("dog.",1001:1500,".jpg")
file.copy(file.path(original_dataset_dir,fnames), file.path(validation_dogs_dir))
fnames<-paste0("dog.",1501:2000,".jpg")          
file.copy(file.path(original_dataset_dir,fnames),file.path(test_dogs_dir))
#Check how many pictures are in each training split
cat("total training cat images:", length(list.files(train_cats_dir)),"\n")
cat("total training dog images:", length(list.files(train_dogs_dir)),"\n")
cat("total validation cat images:",length(list.files(validation_cats_dir)),"\n")
cat("total validation dog images:",length(list.files(validation_dogs_dir)))
cat("total test cat images:", length(list.files(test_cats_dir)),"\n")
cat("total test dog images:", length(list.files(test_dogs_dir)),"\n")
#Instantiating a small convnet for dogs-versus-cats classification
library(keras)
model<-keras_model_sequential()%>%
  layer_conv_2d(filters=32, kernel_size=c(3,3),activation="relu",input_shape=c(150,150,3))%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_conv_2d(filters=64, kernel_size = c(3,3), activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters=128, kernel_size=c(3,3), activation = "relu")%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_flatten()%>%
  layer_dense(units=512,activation = "relu")%>%
  layer_dense(units = 1, activation = "sigmoid")
summary(model)
#Configuring the model for training
model%>%compile(
  loss="binary_crossentropy",
  optimizer=optimizer_rmsprop(lr=1e-4),
  metrics=c("acc")
)
#Transform data into floating-point tensors
train_datagen<-image_data_generator(rescale=1/255)
validation_datagen<-image_data_generator(rescale = 1/255)
train_generator<-flow_images_from_directory(
  train_dir,
  train_datagen,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)
validation_generator<-flow_images_from_directory(
  validation_dir,
  validation_datagen,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)
batch<-generator_next(train_generator)
str(batch)
#Fitting the model using a batch generator
history<-model%>%
  fit_generator(
    train_generator,
    steps_per_epoch = 20,
    epochs=10,
    validation_data = validation_generator,
    validation_steps = 50
  )
#Plot the model
plot(history)
#Setting up a data augmentation configuration via image_data_generator
datagen<-image_data_generator(
  rescale = 1/255,
  rotation_range=40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest"
)
#Displaying some randomly augmented training images
fnames<-list.files(train_cats_dir, full.names = TRUE)
img_path<-fnames[[3]]
img<-image_load(img_path, target_size=c(150,150))
img_array<-image_to_array(img)
img_array<-array_reshape(img_array, c(1,150,150,3))
augmentation_generator<-flow_images_from_data(
  img_array,
  generator = datagen,
  batch_size = 1
)
op<-par(mfrow=c(2,2), pty="s", mar=c(1,0,1,0))
for(i in 1:4){
  batch<-generator_next(augmentation_generator)
  plot(as.raster(batch[1,,,]))
}
par(op)
#Defining a new convnet that includes dropout
model<-keras_model_sequential()%>%
  layer_conv_2d(filters=32, kernel_size = c(3,3), activation = "relu", input_shape = c(150,150,3))%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu")%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_flatten()%>%
  layer_dropout(rate=0.5)%>%
  layer_dense(units=512, activation = "relu")%>%
  layer_dense(units=1, activation = "sigmoid")
model%>%compile(
  loss="binary_crossentropy",
  optimizer=optimizer_rmsprop(lr=1e-4),
  metrics=c("acc")
)
#Training the convnet using data-augmentation generators
datagen<-image_data_generator(
  rescale=1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE
)
test_datagen<-image_data_generator(rescale = 1/255)
train_generator<-flow_images_from_directory(
  train_dir,
  datagen,
  target_size = c(150,150),
  batch_size = 32,
  class_mode = "binary"
)
validation_generator<-flow_images_from_directory(
  validation_dir,
  test_datagen,
  target_size = c(150,150),
  batch_size = 32,
  class_mode="binary"
)
history<-model%>%
  fit_generator(
    train_generator,
    steps_per_epoch = 10,
    epochs = 10,
    validation_data = validation_generator,
    validation_steps = 10
  )
plot(history)
#Save the model
model %>% save_model_hdf5("cats_and_dogs_small_2.h5")

##Using A Pretrained Convnet##
library(keras)
conv_base<-application_vgg16(
  weights="imagenet",
  include_top = FALSE,
  input_shape=c(150,150,3)
)
summary(conv_base)
#Extracting features using the pretrained convolutional base
base_dir<-"~/Desktop/Database/dogs vs cats/cats_and_dogs_small"
train_dir<-file.path(base_dir, "train")
validation_dir<-file.path(base_dir, "validation")
test_dir<-file.path(base_dir,"test")
datagen<-image_data_generator(rescale = 1/255)
batch_size<-20
extract_features<-function(directory, sample_count){
  features<-array(0, dim=c(sample_count, 4,4,512))
  labels<-array(0, dim=c(sample_count))
  generator<-flow_images_from_directory(
    directory=directory,
    generator = datagen,
    target_size=c(150,150),
    batch_size = batch_size,
    class_mode = "binary"
  )
  i<-0
  while(TRUE){
    batch<-generator_next(generator)
    inputs_batch<-batch[[1]]
    labels_batch<-batch[[2]]
    features_batch<-conv_base%>%predict(inputs_batch)
    index_range<-((i*batch_size)+1):((i+1)*batch_size)
    features[index_range,,,]<-features_batch
    labels[index_range]<-labels_batch
    i<-i+1
    if (i*batch_size>=sample_count)
      break
  }
  list(
    features=features,
    labels=labels
  )
}
train<-extract_features(train_dir,2000)
validation<-extract_features(validation_dir,1000)
test<-extract_features(test_dir,1000)
#Reshape the features
reshape_features<-function(features){
  array_reshape(features, dim=c(nrow(features),4*4*512))
}
train$features<-reshape_features(train$features)
validation$features<-reshape_features(validation$features)
test$features<-reshape_features(test$features)
#Defining and training the densely connected classifier
model<-keras_model_sequential()%>%
  layer_dense(units=256, activation="relu", input_shape = 4*4*512)%>%
  layer_dropout(rate=0.5)%>%
  layer_dense(units=1, activation = "sigmoid")
model%>%compile(
  optimizer=optimizer_rmsprop(lr=2e-5),
  loss="binary_crossentropy",
  metrics=c("accuracy")
)
history<-model%>%fit(
  train$features,train$labels,
  epochs=30,
  batch_size=20,
  validation_data=list(validation$features,validation$labels)
)
plot(history)
#Adding a densely connected classifier on top of the convolutional base
model<-keras_model_sequential()%>%
  conv_base%>%
  layer_flatten()%>%
  layer_dense(units=256, activation = "relu")%>%
  layer_dense(units=1, activation = "sigmoid")
#Freeze convolutional base
freeze_weights(conv_base)
#Training the model end to end iwth a fronzen convolutional base
train_datagen=image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode="nearest"
)
test_datagen<-image_data_generator(rescale = 1/255)
train_generator<-flow_images_from_directory(
  train_dir,
  train_datagen,
  target_size = c(150,150),
  batch_size=20,
  class_mode="binary"
)
validation_generator<-flow_images_from_directory(
  validation_dir,
  test_datagen,
  target_size = c(150,150),
  batch_size=20,
  class_mode = "binary"
)
model%>%compile(
  loss="binary_crossentropy",
  optimizer=optimizer_rmsprop(lr=2e-5),
  metrics=c("accuracy")
)
history<-model%>%fit_generator(
  train_generator,
  steps_per_epoch = 50,
  epochs=30,
  validation_data = validation_generator,
  validation_steps = 50
)
plot(history)
#Unfreeze previously frozen layers
unfreeze_weights(conv_base, from="block3_conv1")
#Fine-tuning the model
model%>%compile(
  loss="binary_crossentropy",
  optimizer=optimizer_rmsprop(lr=1e-5),
  metrics=c("accuracy")
)
history<-model%>%fit_generator(
  train_generator,
  steps_per_epoch = 10,
  epochs = 10,
  validation_data = validation_generator,
  validation_steps = 10
)
#Evaluate the model on test data
test_generator<-flow_images_from_directory(
  test_dir,
  test_datagen,
  target_size=c(150,150),
  batch_size = 20,
  class_mode = "binary"
)
model%>%evaluate_generator(test_generator,steps=50)


##Visualizing intermediate activations##
#Load the model
library(keras)
model<-load_model_hdf5("cats_and_dogs_small_2.h5")
model
#Preprocessing a single image
img_path<-"~/Desktop/Database/dogs vs cats/cats_and_dogs_small/test/cats/cat.1700.jpg"
img<-image_load(img_path, target_size = c(150,150))
img_tensor<-image_to_array(img)
img_tensor<-array_reshape(img_tensor,c(1,150,150,3))
img_tensor<-img_tensor/255
dim(img_tensor)
#Displaying the test picture
plot(as.raster(img_tensor[1,,,]))
#Instantiating a model from an inpu tensor and a list of output tensors
layer_ouputs<-lapply(model$layers[1:8],function(layer) layer$output)
activation_model<-keras_model(inputs=model$input, outputs = layer_ouputs)
#Running the model in predict mode
activations<-activation_model%>%predict(img_tensor)
#Activation of the first convolution
first_layer_activation<-activations[[1]]
#Function to plot a channel
plot_channel<-function(channel){
  rotate<-function(x) t(apply(x,2,rev))
  image(rotate(channel), axes=FALSE, asp=1, col=terrain.colors(12))
}
#Plotting the second channel
plot_channel(first_layer_activation[1,,,2])
#Visualizing the seventh channel
plot_channel(first_layer_activation[1,,,7])
#Visualizing every channel in every intermediate activation
image_size<-58
images_per_row<-16
for (i in 1:8){
  layer_activation<-activations[[i]]
  layer_name<-model$layers[[i]]$name
  n_features<-dim(layer_activation)[[4]]
  n_cols<-n_features%/%images_per_row
  png(paste0("cat_activations_", i, "_",layer_name,".png"),
      width=image_size*images_per_row,
      height=image_size*n_cols)
  op<-par(mfrow=c(n_cols, images_per_row), mai=rep_len(0.02,4))
  for(col in 0:(n_cols-1)){
    for(row in 0:(images_per_row-1)){
      channel_image<-layer_activation[1,,,(col*images_per_row)+row+1]
      plot_channel(channel_image)
    }
  }
  par(op)
  dev.off()
}


##Working with Text Data##
#Using Keras for word-level one-hot encoding
library(keras)
samples<-c("The cat sat on the mat.","The dog ate my homework")
tokenizer<-text_tokenizer(num_words=1000)%>%
  fit_text_tokenizer(samples)
sequences<-texts_to_sequences(tokenizer, samples)
one_hot_results<-texts_to_matrix(tokenizer, samples, mode="binary")
word_index<-tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")

#World-level one-hot encoding with hasing trick
library(hashFunction)
samples<-c("The cat sat on the mat.","The dog ate my homework.")
dimensionality<-1000
max_length<-10
results<-array(0, dim=c(length(samples), max_length, dimensionality))
for (i in 1:length(samples)){
  sample<-samples[[i]]
  words<-head(strsplit(sample," ")[[1]],n=max_length)
  for (j in 1:length(words)){
    index<-abs(spooky.32(words[i]))%%dimensionality
    results[[i,j,index]]<-1
  }
}

#Loading the IMDB data for use with an embedding layer
max_features<-10000
maxlen<-20
imdb<-dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test))%<-%imdb
x_train<-pad_sequences(x_train, maxlen = maxlen)
x_test<-pad_sequences(x_test,maxlen=maxlen)
#Using an embedding layer and classifier on the IMDB data
model<-keras_model_sequential()%>%
  layer_embedding(input_dim=10000, output_dim=8, input_length = maxlen)%>%
  layer_flatten()%>%
  layer_dense(units=1, activation = "sigmoid")
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("acc")
)
summary(model)
history<-model%>%fit(
  x_train,y_train,
  epochs=10,
  batch_size=32,
  validation_split=0.2
)

##Processing the labels of the raw IMDB data##(Abandon)
#only need to install onece: devtools::install_github("rstudio/keras")
imdb_dir<-"~/Downloads/aclImdb"
train_dir<-file.path(imdb_dir,"train")
labels<-c()
texts<-c()
for (label_type in c("neg","pos")){
  label<-switch(label_type, neg=0, pos=1)
  dir_name<-file.path(train_dir, label_type)
  for (fname in list.files(dir_name, pattern = glob2rx("*.txt"),
                           full.names = TRUE)){
    texts<-c(texts, readChar(fname, file.info(fname)$size))
    labels<-c(labels, label)
  }
}
#Tokenizing the text of raw IMDB data
library(keras)
maxlen<-100
training_samples<-200
validation_samples<-10000
max_words<-10000
tokenizer<-text_tokenizer(num_words = max_words)%>%fit_text_tokenizer(texts)
sequences<-texts_to_sequences(tokenizer, texts)


##R Implementation of a simple RNN##
timesteps<-100
input_features<-32
output_features<-64
random_array<-function(dim){
  array(runif(prod(dim)), dim=dim)
}
inputs<-random_array(dim=c(timesteps, input_features))
state_t<-rep_len(0, length=c(output_features))
w<-random_array(dim=c(output_features, input_features))
u<-random_array(dim = c(output_features,output_features))
b<-random_array(dim=c(output_features,1))
output_sequence<-array(0, dim=c(timesteps, output_features))
for (i in 1:nrow(inputs)){
  input_t<-inputs[i,]
  output_t<-tanh(as.numeric((w%*%input_t)+(u %*% state_t)+b))
  output_sequence[i,]<-as.numeric(output_t)
  state_t<-output_t
}
output_t<-tanh(as.numeric((w%*%input_t)+(u%*%state_t)+b))
output_t


##Simple Use of layer_simple_rnn##
#Returns only the output at the last timestep
library(keras)
model<-keras_model_sequential()%>%
  layer_embedding(input_dim=10000, output_dim = 32)%>%
  layer_simple_rnn(units=32)
summary(model)

#Reuturns the full state sequence
model<-keras_model_sequential()%>%
  layer_embedding(input_dim=10000, output_dim = 32)%>%
  layer_simple_rnn(units=32, return_sequences = TRUE)
summary(model)

#Stack several recurrent layers
model<-keras_model_sequential()%>%
  layer_embedding(input_dim=10000, output_dim = 32)%>%
  layer_simple_rnn(units = 32, return_sequences = TRUE)%>%
  layer_simple_rnn(units=32, return_sequences = TRUE)%>%
  layer_simple_rnn(units=32, return_sequences = TRUE)%>%
  layer_simple_rnn(units=32)
summary(model)


##Training imdb with embedding and simple RNN layers##
#Preparing the IMDB data
library(keras)
max_features<-10000
maxlen<-500
batch_size<-32
cat("loading data...\n")
imdb<-dataset_imdb(num_words = max_features)
c(c(input_train, y_train), c(input_test, y_test)) %<-% imdb
cat(length(input_train), "train sequences\n")
cat(length(input_test), "test sequences")
cat("Pad sequences (samples x time)\n")
input_train<-pad_sequences(input_train, maxlen = maxlen)
input_test<-pad_sequences(input_test, maxlen = maxlen)
cat("input_train shape:", dim(input_train), "\n")
cat("input_test shape:", dim(input_test), "\n")
#Training the model with embedding and simple RNN layers
model<-keras_model_sequential()%>%
  layer_embedding(input_dim = max_features, output_dim = 32)%>%
  layer_simple_rnn(units=32)%>%
  layer_dense(units=1, activation = "sigmoid")
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("acc")
)
history<-model%>%fit(
  input_train, y_train,
  epochs=10,
  batch_size=128,
  validation_split=0.2
)
plot(history)


##Using the LSTM layer in Keras##
#Prepare the imdb dataset
library(keras)
max_features<-10000
maxlen<-500
batch_size<-32
imdb<-dataset_imdb(num_words = max_features)
c(c(input_train, y_train), c(input_test, y_test)) %<-% imdb
input_train<-pad_sequences(input_train, maxlen = maxlen)
input_test<-pad_sequences(input_test, maxlen = maxlen)
#Training the LSTM model
model<-keras_model_sequential()%>%
  layer_embedding(input_dim=max_features, output_dim = 32)%>%
  layer_lstm(units=32)%>%
  layer_dense(units=1, activation = "sigmoid")
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("acc")
)
history<-model%>%fit(
  input_train, y_train,
  epochs=10,
  batchs=10,
  batch_size=128,
  validation_split=0.2
)
plot(history)


##A temperature-forecasting problem##
#Download the dataset
dir.create("~/Downloads/jena_climate", recursive = TRUE)
download.file("https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip","~/Downloads/jena_climate/jena_climate_2009_2016.csv.zip")
unzip( "~/Downloads/jena_climate/jena_climate_2009_2016.csv.zip", exdir = "~/Downloads/jena_climate"
)
#Inspecting the data of the Jena Weather dataset
library(tibble)
library(readr)
data_dir<-"~/Downloads/jena_climate"
fname<-file.path(data_dir, "jena_climate_2009_2016.csv")
data<-read_csv(fname)
glimpse(data)
#Plotting the temperature timeseries
library(ggplot2)
ggplot(data, aes(x=1:nrow(data), y='T(degC)'))+geom_line()
#Temperature over the first 10 days of the dataset
ggplot(data[1:1440,], aes(x=1:1440, y='T(degC)'))+geom_line()
#Generator function
sequence_generator<-function(start){
  value<-start-1
  function(){
    value<<-value+1
    value
  }
}
gen<-sequence_generator(10)
gen()
gen()
#Converting the data into a floating-point matrix
data<-data.matrix(data[,-1])
#Normalizing the data
train_data<-data[1:200000,]
mean<-apply(train_data, 2, mean)
std<-apply(train_data,2,sd)
data<-scale(data, center=mean, scale=std)
#Generator yielding timeseries samples and their targets
generator<-function(data, lookback, delay, min_index, max_index,
                    shuffle=FALSE, batch_size=128, step=6){
  if (is.null (max_index)) max_index<-nrow(data)-delay-1
  i<-min_index+lookback
  function(){
    if (shuffle){
      rows<-sample(c((min_index+lookback):max_index),size=batch_size)
    } else{
      if (i+batch_size>=max_index)
        i<<-min_index+lookback
      rows<-c(i:min(i+batch_size, max_index))
      i<<-i+length(rows)
    }
    samples<-array(0, dim=c(length(rows),
                            lookback/step,
                            dim(data)[[-1]]))
    targets<-array(0, dim=c(length(rows)))
    for (j in 1:length(rows)){
      indices<-seq(rows[[j]]-lookback, rows[[j]],
                   length.out=dim(samples)[[2]])
      samples[j,,]<-data[indices,]
      targets[[j]]<-data[rows[[j]]+delay,2]
    }
    list(samples, targets)
  }
}
#Preparing the training, validation and test generators
library(keras)
lookback<-1440
step<-6
delay<-144
batch_size<-128
train_gen<-generator(
  data,
  lookback=lookback,
  delay=delay,
  min_index=1,
  max_index = 200000,
  shuffle=TRUE,
  step=step,
  batch_size = batch_size
)
val_gen=generator(
  data,
  lookback=lookback,
  delay=delay,
  min_index=200001,
  max_index=300000,
  step=step,
  batch_size=batch_size
)
test_gen<-generator(
  data,
  lookback=lookback,
  delay=delay,
  min_index=300001,
  max_index=NULL,
  step=step,
  batch_size=batch_size
)
val_steps<-(300000-200001-lookback)/batch_size
test_steps<-(nrow(data)-300001-lookback)/batch_size
#Computing the common-sense baseline MAE
evaluate_naive_method<-function(){
  batch_maes<-c()
  for (step in 1:val_steps){
    c(samples, targets) %<-% val_gen()
    preds<-samples[,dim(samples)[[2]],2]
    mae<-mean(abs(preds-targets))
    batch_maes<-c(batch_maes, mae)
  }
  print(mean(batch_maes))
}
evaluate_naive_method()
#Converting the MAE back to a Celsius error
celsius_mae<-0.29*std[[2]]
#Training and evaluating a densely connected model
library(keras)
model<-keras_model_sequential()%>%
  layer_flatten(input_shape = c(lookback/step, dim(data)[-1]))%>%
  layer_dense(units=32, activation = "relu")%>%
  layer_dense(units=1)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs=20,
  validation_data = val_gen,
  validation_steps = val_steps
)
plot(history)

#Training and evaluating a model with layer_gru
model<-keras_model_sequential()%>%
  layer_gru(units=32, input_shape=list(NULL, dim(data)[[-1]]))%>%
  layer_dense(units=1)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs=5,
  validation_data = val_gen,
  validation_steps = val_steps
)

#Training and evaluating a dropout-regularized GRU-based model
model<-keras_model_sequential()%>%
  layer_gru(units=32, dropout = 0.2, recurrent_dropout = 0.2,
            input_shape=list(NULL, dim(data)[[-1]]))%>%
  layer_dense(units=1)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%fit_generator(
  train_gen,
  steps_per_epoch = 50,
  epochs=10,
  validation_data = val_gen,
  validation_steps = val_steps
)
plot(history)

#Training and evaluating a dropout-regularzied, stacked GRU model
model<-keras_model_sequential()%>%
  layer_gru(units=32,
            dropout = 0.1,
            recurrent_dropout = 0.5,
            return_sequences = TRUE,
            input_shape = list(NULL, dim(data)[[-1]]))%>%
  layer_gru(units = 64, activation = "relu",
            dropout=0.1,
            recurrent_dropout = 0.5)%>%
  layer_dense(units=1)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%fit_generator(
  train_gen,
  steps_per_epoch = 50,
  epochs=10,
  validation_data = val_gen,
  validation_steps = val_steps
)
plot(history)

#Training and evaluating an LSTM using reversed sequences
library(keras)
max_features<-100000
maxlen<-500
imdb<-dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test))%<-%imdb
x_train<-lapply(x_train, rev)
x_test<-lapply(x_test, rev)
x_train<-pad_sequences(x_train, maxlen = maxlen)
x_test<-pad_sequences(x_test, maxlen=maxlen)
model<-keras_model_sequential()%>%
  layer_embedding(input_dim = max_features, output_dim=128)%>%
  layer_lstm(units=32)%>%
  layer_dense(units=1, activation = "sigmoid")
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("acc")
)
history<-model%>%fit(
  x_train, y_train,
  epochs=10,
  batch_size=128,
  validation_split=0.2
)
plot(history)


##1D convolution##
#Preparing the IMDB data
library(keras)
max_features<-10000
max_len<-500
cat("Loading data...\n")
imdb<-dataset_imdb(num_words = max_features)
c(c(x_train, x_test),c(y_train, y_test))%<-%imdb
cat(length(x_train),"train sequences\n")
cat(length(x_test),"test sequences\n")
cat("Pad sequences (samples x time)\n")
#Training and evaluating a simple 1D convnet on the IMDB data
model<-keras_model_sequential()%>%
  layer_embedding(input_dim=max_features, output_dim = 128,
                  input_length=max_len)%>%
  layer_conv_1d(filters=32, kernel_size = 7, activation = "relu")%>%
  layer_max_pooling_1d(pool_size = 5)%>%
  layer_conv_1d(filters=32, kernel_size = 7, activation = "relu")%>%
  layer_global_max_pooling_1d()%>%
  layer_dense(units=1)
summary(model)  
model%>%compile(
  optimizer=optimizer_rmsprop(lr=1e-4),
  loss="binary_crossentropy",
  metrics=c("acc")
)
history<-model%>%fit(
  x_train, y_train,
  epochs=10,
  batch_size=128,
  validation_split=0.2
)

#Training and evaluating a simple 1D convnet on the Jena data
model<-keras_model_sequential()%>%
  layer_conv_1d(filters=32, kernel_size = 5, activation = "relu",
                input_shape=list(NULL, dim(data)[[-1]]))%>%
  layer_max_pooling_1d(pool_size=3)%>%
  layer_conv_1d(filters=32, kernel_size = 5, activation = "relu")%>%
  layer_max_pooling_1d(pool_size = 3)%>%
  layer_conv_1d(filters=32, kernel_size = 5, activation = "relu")%>%
  layer_global_max_pooling_1d()%>%
  layer_dense(units=1)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%fit_generator(
  train_gen,
  steps_per_epoch = 20,
  epochs=20,
  validation_data = val_gen,
  validation_steps = val_steps
)
plot(history)

#Preparing higher-resolution data generators for the Jena Dataset
data_dir<-"~/Downloads/jena_climate"
fname<-file.path(data_dir, "jena_climate_2009_2016.csv")
data<-read_csv(fname)
step<-3
lookback<-720
delay<-144
train_gen<-generator(
  data,
  lookback=lookback,
  delay=delay,
  min_index=1,
  max_index=200000,
  shuffle=TRUE,
  step=step
)
val_gen<-generator(
  data,
  lookback=lookback,
  delay=delay,
  min_index=200001,
  max_index=300000,
  step = step
)
test_gen<-generator(
  data,
  lookback=lookback,
  delay=delay,
  min_index=300001,
  max_index=NULL,
  step = step
)
val_steps<-(300000-200001-lookback)/128
test_steps<-(nrow(data)-300001-lookback)/128
#Model combining a 1D convolutional base and a GRU layer
model<-keras_model_sequential()%>%
  layer_conv_1d(filters=32, kernel_size = 5, activation="relu",
                input_shape=list(NULL, dim(data)[[-1]]))%>%
  layer_max_pooling_1d(pool_size=3)%>%
  layer_conv_1d(filters=32, kernel_size = 5, activation = "relu")%>%
  layer_gru(units=32, dropout=0.1, recurrent_dropout = 0.5)%>%
  layer_dense(units=1)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs=20,
  validation_data=val_gen,
  validation_steps = val_steps
)


##Function API Implementation of a two-input question-answering model##
library(keras)
text_vocabulary_size<-10000
ques_vocabulary_size<-10000
answer_vocabulary_size<-500
text_input<-layer_input(shape=list(NULL), dtype="int32",name="text")
encoded_text<-text_input%>%
  layer_embedding(input_dim = 64, output_dim=text_vocabulary_size)%>%
  layer_lstm(units=32)
question_input<-layer_input(shape=list(NULL),dtype="int32",name="question")
encoded_question<-question_input%>%
  layer_embedding(input_dim=32, output_dim = ques_vocabulary_size)%>%
  layer_lstm(units=16)
concatenated<-layer_concatenate(list(encoded_text, encoded_question))
answer<-concatenated%>%
  layer_dense(units=answer_vocabulary_size, activation = "softmax")
model<-keras_model(list(text_input, question_input), answer)
model%>%compile(
  optimizer="rmsprop",
  loss="categorical_crossentropy",
  metrics=c("acc")
)
#Feeding data into a multi-input model
num_samples<-1000
max_length<-100
random_matrix<-function(range, nrow, ncol){
  matrix(sample(range, size=nrow*ncol, replace=TRUE),
                nrow=nrow, ncol=ncol)
}
text<-random_matrix(1:text_vocabulary_size, num_samples, max_length)
question<-random_matrix(1:ques_vocabulary_size,num_samples, max_length)
answers<-random_matrix(0:1, num_samples, answer_vocabulary_size)
model%>%fit(
  list(text, question), answers,
  epochs=10, batch_size=128
)
model%>%fit(
  list(text=text, question=question), answers,
  epochs=10, batch_size=128
  )

#Functional API Implementation of a three-output model
library(keras)
vocabulary_size<-50000
num_income_groups<-10
post_input<-layer_input(shape=list(NULL),
                         dtype="int32",
                         name="posts")
embedded_posts<-post_input%>%
  layer_embedding(input_dim=256, output_dim=vocabulary_size)
base_model<-embedded_posts%>%
layer_conv_1d(filters=128, kernel_size=5, activation = "relu")%>%
  layer_max_pooling_1d(pool_size = 5)%>%
  layer_conv_1d(filters = 256, kernel_size=5, activation="relu")%>%
  layer_conv_1d(filters=256, kernel_size = 5, activation = "relu")%>%
  layer_max_pooling_1d(pool_size=5)%>%
  layer_conv_1d(filters=256, kernel_size = 5, activation = "relu")%>%
  layer_conv_1d(filters=256, kernel_size = 5, activation = "relu")%>%
  layer_global_max_pooling_1d()%>%
  layer_dense(units=128, activation = "relu")
age_prediction<-base_model%>%
  layer_dense(units=1, name="age")
income_prediction<-base_model%>%
  layer_dense(num_income_groups, activation = "softmax", name="income")
gender_prediction<-base_model%>%
  layer_dense(units=1, activation = "sigmoid", name="gender")
model<-keras_model(
  posts_input,
  list(age_prediction, income_prediction, gender_prediction)
)
