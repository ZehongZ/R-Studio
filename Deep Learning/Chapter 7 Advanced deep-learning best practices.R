#Simple sequential model and its equivalent in the functional API
library(keras)
seq_model<-keras_model_sequential()%>%
  layer_dense(units=32, activation = "relu", input_shape = c(64))%>%
  layer_dense(units=32, activation = "relu")%>%
  layer_dense(units=10, activation = "softmax")
input_tensor<-layer_input(shape=c(64))
output_tensor<-input_tensor%>%
  layer_dense(units=32, activation = "relu")%>%
  layer_dense(units=32, activation = "relu")%>%
  layer_dense(units=10, activation = "softmax")
model<-keras_model(input_tensor,output_tensor)
summary(model)

#Functional API implementation of a two-input question-answering model
library(keras)
text_vocabulary_size<-10000
ques_vocabulary_size<-10000
answer_vocabulary_size<-500
text_input<-layer_input(shape=list(NULL), dtype="int32",name="text")
encoded_text<-text_input%>%
  layer_embedding(input_dim = 64, output_dim = text_vocabulary_size)%>%
  layer_lstm(units=32)
question_input<-layer_input(shape=list(NULL), dtype="int32",name="question")
encoded_question<-question_input%>%
  layer_embedding(input_dim = 32, output_dim = ques_vocabulary_size)%>%
  layer_lstm(units=16)
concatenated<-layer_concatenate(list(encoded_text,encoded_question))
answer<-concatenated%>%
  layer_dense(units=answer_vocabulary_size, activation = "softmax")
model<-keras_model(list(text_input, question_input),answer)
model%>%compile(
  optimizer="rmsprop",
  loss="categorical_crossentropy",
  metrics=c("acc")
)

#Feeding data to a multi-input model
num_samples<-1000
max_length<-100
random_matrix<-function(range, nrow, ncol){
  matrix(sample(range, size=nrow*ncol, replace=TRUE),nrow=nrow, ncol=ncol)
}
text<-random_matrix(1:text_vocabulary_size, num_samples, max_length)
question<-random_matrix(1:ques_vocabulary_size, num_samples, max_length)
answers<-random_matrix(0:1, num_samples, answer_vocabulary_size)
model%>%fit(
  list(text, question),answers,
  epochs=10, batch_size=128
)
model%>%fit(
  list(text=text, question=question),answers,
  epochs=10, batch_size=128
)

#Functional API implementation of a three-output model
library(keras)
vocabulary_size<-50000
num_income_groups<-10
posts_input<-layer_input(shape=list(NULL),dtype="int32",name="posts")
embedded_posts<-posts_input%>%
  layer_embedding(input_dim=256, output_dim=vocabulary_size)
base_model<-embedded_posts%>%
  layer_conv_1d(filters=128, kernel_size = 5, activation="relu")%>%
  layer_max_pooling_1d(pool_size = 5)%>%
  layer_conv_1d(filters = 256, kernel_size = 5, activation = "relu")%>%
  layer_conv_1d(filters=256, kernel_size = 5, activation = "relu")%>%
  layer_max_pooling_1d(pool_size = 5)%>%
  layer_conv_1d(filters = 256, kernel_size = 5, activation = "relu")%>%
  layer_conv_1d(filters = 256, kernel_size = 5, activation = "relu")%>%
  layer_global_max_pooling_1d()%>%
  layer_dense(units=128, activation = "relu")
age_prediction<-base_model%>%
  layer_dense(units=1, name="age")
income_prediction<-base_model%>%
  layer_dense(num_income_groups, activation = "softmax",name="income")
gender_prediction<-base_model%>%
  layer_dense(units = 1, activation = "sigmoid",name="gender")
model<-keras_model(
  posts_input,
  list(age_prediction, income_prediction, gender_prediction)
)

#Compliation options of a multi-output model:multiple losses
model%>%compile(
  optimizer="rmsprop",
  loss=c("mse","categorical_crossentropy","binary_crossentropy")
)
model%>%compile(
  optimizer="rmsprop",
  loss=list(
    age="mse",
    income="categorical_crossentropy",
    gender="binary_crossentropy"
  )
)

#Compliation options of a multi-output model:loss weighting
model%>%compile(
  optimizer="rmsprop",
  loss=c("mse","categorical_crossentropy","binary_crossentropy"),
  loss_weights=c(0.25,1,10)
)
model%>%compile(
  optimizer="rmsprop",
  loss=list(
    age="mse",
    income="categorical_crossentropy",
    gender="binary_crossentropy"
  ),
  loss_weights=list(
    age=0.25,
    income=1,
    gender=10
  )
)

#Feeding data to a multi-output model
model%>%fit(
  posts, list(age_targets, income_targets, gender_targets),
  epochs=10,
  batch_size=64
)
model%>%fit(
  posts, list(
    age=age_targets,
    income=income_targets,
    gender=gender_targets
  ),
  epochs=10, batch_size=64
)

#Implement a residual connection in Keras when the feature-map sizes are the same, using identity residual connections
output<-input%>%
  layer_conv_2d(filters = 128, kernel_size = 3, activation = "relu", padding = "same")%>%
  layer_conv_2d(filters = 128, kernel_size = 3, activation = "relu", padding = "same")%>%
  layer_conv_2d(filters=128, kernel_size = 3, activation = "relu", padding = "same")
output<-layer_add(list(output,input))

#Implements a residual connection when the feature-map sizes differ, using a linear residual connection
output<-input%>%
  layer_conv_2d(filters = 128, kernel_size = 3, activation = "relu", padding="same")%>%
  layer_conv_2d(filter(128, kernel_size=3, activation="relu", padding="same"))%>%
  layer_max_pooling_2d(pool_size = 2, strides=2)
residual<-input%>%
  layer_conv_2d(filters=128, kernel_size = 1, strides = 2, padding = "same")
output<-layer_add(list(output, residual))

#Layer sharing
library(keras)
lstm<-layer_lstm(units=32)
left_input<-layer_input(shape=list(NULL,128))
left_output<-left_input%>%lstm()
right_input<-layer_input(shape=list(NULL,128))
right_output<-right_input%>%lstm()
merged<-layer_concatenate(list(left_output,right_output))
predictions<-merged%>%
  layer_dense(units = 1, activation = "sigmoid")
model<-keras_model(list(left_input, right_input), predictions)
model%>%fit(
  list(left_data, right_data),targets
)

#Siamese vision model in Keras
library(keras)
xception_base<-application_xception(weights=NULL, include_top=FALSE)
left_input<-layer_input(shape=c(250, 250, 3))
right_input<-layer_input(shape=c(250, 250, 3))
left_features<-left_input%>%xception_base()
right_features<-right_input%>%xception_base()
merged_features<-layer_concatenate(
  list(left_features, right_features)
)
