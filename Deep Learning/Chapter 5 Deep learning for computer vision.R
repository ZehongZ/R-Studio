library(keras)
model<-keras_model_sequential()%>%
  layer_conv_2d(filters=32, kernel_size = c(3,3),activation="relu",
                input_shape = c(28,28,1))%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")
model<-model%>%
  layer_flatten()%>%
  layer_dense(units=64, activation = "relu")%>%
  layer_dense(units=10, activation = "softmax")
model
mnist<-dataset_mnist()
c(c(train_images, train_labels), c(test_images, test_labels))%<-%mnist
train_images<-array_reshape(train_images, c(60000,28,28,1))
train_images<-train_images/255
test_images<-array_reshape(test_images,c(10000,28,28,1))
test_images<-test_images/255
train_labels<-to_categorical(train_labels)
test_labels<-to_categorical(test_labels)
model%>%compile(
  optimizer="rmsprop",
  loss="categorical_crossentropy",
  metrics=c("accuracy")
)
model%>%fit(
  train_images,train_labels,
  epochs=5, batch_size=64
)
results<-model%>%evaluate(test_images,test_labels)
results#The basic convnet has increased the accuracy and decrease the error rate

#Cat vs Dog calssification 
#Copy images to training, validation and test directories
original_dir<-file.path("dogs-vs-cats-train-validation-and-evaluation")
train_dir<-file.path("train")
validation_dir<-file.path("validation")
test_dir<-file.path("evaluation")
train_cats_dir<-file.path(train_dir,"cat")
dir.create(train_cats_dir)
train_dogs_dir<-file.path(train_dir,"dog")
dir.create(train_dogs_dir)
validation_cats_dir<-file.path(validation_dir, "cat")
dir.create(validation_cats_dir)
validation_dogs_dir<-file.path(validation_dir, "dog")
dir.create(validation_dogs_dir)
test_cats_dir<-file.path(test_dir,"cat")
dir.create(test_cats_dir)
test_dogs_dir<-file.path(test_dir,"dog")
dir.create(test_dogs_dir)
fnames<-paste0("cat.", 1:1000, ".jpg")
file.copy(file.path(original_dir, fnames),
          file.path(train_cats_dir))
fnames<-paste0("cat.", 1001:1500, ".jpg")
file.copy(file.path(original_dir, fnames),
          file.path(train_cats_dir))
fnames<-paste0("cat.", 1501:2000, ".jpg")
file.copy(file.path(original_dir, fnames),
          file.path(train_cats_dir))
cat("Total training cat images:", length(list.files(train_cats_dir)))
fnames<-paste0("dog.",1:1000,".jpg")
file.copy(file.path(original_dir,fnames),
          file.path(train_dogs_dir))
fnames<-paste0("dog.",1001:1500,".jpg")
file.copy(file.path(original_dir,fnames),
          file.path(train_dogs_dir))
fnames<-paste0("dog.",1501:2000,".jpg")
file.copy(file.path(original_dir,fnames),
          file.path(train_dogs_dir))
cat("Total training dog images:", length(list.files(train_dogs_dir)))
cat("Total validation cat images:", length(list.files(validation_cats_dir)))
cat("Total validation dog images:", length(list.files(validation_dogs_dir)))
cat("Total test cat images:",length(list.files(test_cats_dir)))
cat("Total test dog images:",length(list.files(test_dogs_dir)))
#Instantiating a small convnet for dogs-versus-cats classification
library(keras)
model<-keras_model_sequential()%>%
  layer_conv_2d(filters=32, kernel_size = c(3,3),activation = "relu",
                input_shape=c(150,150,3))%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation="relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_flatten()%>%
  layer_dense(units=512, activation = "relu")%>%
  layer_dense(units=1, activation = "sigmoid")
summary(model)
#Configuring the model for training
model%>%compile(
  loss="binary_crossentropy",
  optimizer=optimizer_rmsprop(lr=1e-4),
  metrics=c("acc")
)
#Using image_data_genarator to read images from directories
train_datagen<-image_data_generator(rescale = 1/255)#Rescales all images by 1/255
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
    steps_per_epoch = 100,
    epochs=30,
    validation_data=validation_generator,
    validation_steps = 50
  )
#Saving the model
model%>%save_model_hdf5("Cats_and_Dogs_small_1.h5")
plot(history)
#Settting up a data augmentation configuration via image_data_generator
datagen<-image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest"
)
#Displaying some randomly augmented training images
fnames<-list.files(train_cats_dir,full.names = TRUE)
img_path<-fnames[[3]]
img<-image_load(img_path, target_size = c(150,150))
img_array<-image_to_array(img)
img_array<-array_reshape(img_array,c(1,150,150,3))
augmentation_generator<-flow_images_from_data(
  img_array,
  generator = datagen,
  batch_size = 1
)
op<-par(mfrow=c(2,2),pty="s",mar=c(1,0,1,0))
for (i in 1:4){
  batch<-generator_next(augmentation_generator)
  plot(as.raster(batch[1,,,]))
}
par(op)
#Defining a new convnet that includes dropout
model<-keras_model_sequential()%>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu",
                input_shape = c(150,150,3))%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters=128, kernel_size = c(3,3),activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_flatten()%>%
  layer_dropout(rate=0.5)%>%
  layer_dense(units=512, activation = "relu")%>%
  layer_dense(units = 1, activation = "sigmoid")
model%>%compile(
  loss="binary_crossentropy",
  optimizer=optimizer_rmsprop(lr=1e-4),
  metrics=c("acc")
)
#Training the convnet using data-augmentation generators
datagen<-image_data_generator(
  rescale = 1/255,
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
  class_mode = "binary"
)
history<-model%>%fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 10,
  validation_data = validation_generator,
  validation_steps = 30
)
plot(history)

