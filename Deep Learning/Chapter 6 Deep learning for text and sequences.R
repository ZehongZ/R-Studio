#Workd-level one-hot encoding (toy example)
library(keras)
samples<-c("The cat sat on the mat.","The dog ate my homework")
token_index<-list()
for (sample in samples)
  for(word in strsplit(sample, " ")[[1]])
    if(!word %in% names(token_index))
      token_index[[word]]<-length(token_index)+2
max_length<-10
results<-array(0, dim=c(length(samples),
                        max_length,
                        max(as.integer(token_index))))
for(i in 1:length(samples)){
  sample<-samples[[i]]
  words<-head(strsplit(sample, " ")[[1]], n=max_length)
  for (j in 1:length(words)){
    index<-token_index[[words[j]]]
    results[[i,j,index]]<1
  }
}

#Character-level one-hot encoding(toy example)
samples<-c("The cat sat on the mat.","The dog ate my homeowkr.")
ascii_tokens<-c("", sapply(as.raw(c(32:126)), rawToChar))
token_index<-c(1:(length(ascii_tokens)))
names(token_index)<-ascii_tokens
max_length<-50
results<-array(0, dim=c(length(samples),max_length, length(token_index)))
for (in in 1:length(samples)){
  sample<-samples[[i]]
  characters<-strsplit(sample, "")[[1]]
  for (j in 1:length(characters)){
    character<-characters[[j]]
    results[i,j,token_index[[character]]]<-1
  }
}

#Keras has built-in utilities for doing one-hot encoding of text at the word level or character level, starting from raw text data
#Using Keras for word-level one-hot encoding
library(keras)
samples<-c("The cat sat on the mat.","The dog ate my homework.")
tokenizer<-text_tokenizer(num_words = 1000)%>%
  fit_text_tokenizer(samples)
sequences<-texts_to_sequences(tokenizer, samples)
one_hot_results<-texts_to_matrix(tokenizer, samples, mode="binary")
word_index<-tokenizer$word_index
cat("Found",length(word_index),"unique tokens.\n")

#Word-level one-hot encoding with hashing trick
library(hashFunction)
samples<-c("The cat sat on the mat.","The dog ate my homework.")
dimensionality<-1000
max_length<-10
results<-array(0, dim=c(length(samples), max_length,dimensionality))
for (i in 1:length(samples)){
  sample<-samples[[i]]
  words<-head(strsplit(sample," ")[[1]],n=max_length)
  for (j in 1:length(words)){
    index<-abs(spooky.32(words[[i]]))%%dimensionality
    results[[i,j,index]]<-1
  }
}

#Loading the IMDB data for use with a nembedding layer
library(keras)
max_features<-10000
maxlen<-20
imdb<-dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test))%<-%imdb
x_train<-pad_sequences(x_train, maxlen=maxlen)
x_test<-pad_sequences(x_test, maxlen = maxlen)

#Using an embedding layer and classifier on the IMDB data
model<-keras_model_sequential()%>%
  layer_embedding(input_dim = 10000, output_dim = 8, input_length = maxlen)%>%
  layer_flatten()%>%
  layer_dense(units=1, activation = "sigmoid")
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("acc")
)
summary(model)
history<-model%>%fit(
  x_train, y_train,
  epochs=10,
  batch_size=32,
  validation_split=0.2
)

#Processing the labels of the raw IMDB data
train_dir<-file.path("train")
labels<-c()
texts<-c()
for (label_type in c("neg","pos")){
  label<-switch(label_type, neg=0, pos=1)
  dir_name<-file.path(train_dir, label_type)
  for(fname in list.files(dir_name, pattern=glob2rx("*.txt"),
                          full.names = TRUE)){
    texts<-c(texts, readChar(fname, file.info(fname)$size))
    labels<-c(labels, label)
  }
}

#Toekenizing the text of the raw IMDB data
library(keras)
maxlen<-100
training_samples<-200
validation_samples<-10000
max_words<-10000
tokenizer<-text_tokenizer(num_words = max_words)%>%
  fit_text_tokenizer(texts)
sequences<-texts_to_sequences(tokenizer, texts)
word_index=tokenizer$word_index
cat("Found",length(word_index),"unique tokens.\n")
data<-pad_sequences(sequences, maxlen = maxlen)
labels<-as.array(labels)
cat("Shape of data tensor:", dim(data),"\n")
cat("Shape of label tensor:", dim(labels),"\n")
indices<-sample(1:nrow(data))
training_indices<-indices[1:training_samples]
validation_indices<-indices[(training_samples+1):(training_samples+validation_samples)]
x_train<-data[training_indices,]
y_train<-labels[training_indices]
x_val<-data[validation_indices,]
y_val<-labels[validation_indices]

#Parsing the GloVe word-embeddings file
glove_dir="~/Downloads/glove.6B"
lines<-readLines(file.path(glove_dir,"glove.6B.100d.txt"))
embeddings_index<-new.env(hash=TRUE, parent = emptyenv())
for (i in 1:length(lines)){
  line<-lines[[i]]
  values<-strsplit(line, " ")[[1]]
  word<-values[[1]]
  embeddings_index[[word]]<-as.double(values[-1])
}
cat("Found",length(embeddings_index),"word vectors.\n")

#Preparing the GloVe word-embeddings matrix
embedding_dim<-100
embedding_matrix<-array(0, c(max_words, embedding_dim))
for (word in names(word_index)){
  index<-word_index[[word]]
  if (index<max_words){
    embedding_vector<-embeddings_index[[word]]
    if (!is.null(embedding_vector))
      embedding_matrix[index+1,]<-embedding_vector
  }
}

#Model definition
model<-keras_model_sequential()%>%
  layer_embedding(input_dim=max_words, output_dim = embedding_dim,input_length = maxlen)%>%
  layer_flatten()%>%
  layer_dense(units=32, activation = "relu")%>%
  layer_dense(units=1, activation = "sigmoid")
summary(model)

#Loading pretrained word embeddings into the embedding layer
get_layer(model, index=1)%>%
  set_weights(list(embedding_matrix))%>%
  freeze_weights()

#Training and evaluation
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("acc")
)
history<-model%>%fit(
  x_train, y_train,
  epochs=20,
  batch_size=32,
  validation_data=list(x_val, y_val)
)
plot(history)

#Training the same model without pretrained word embeddings
model<-keras_model_sequential()%>%
  layer_embedding(input_dim=max_words, output_dim=embedding_dim,input_length = maxlen)%>%
  layer_flatten()%>%
  layer_dense(units=32, activation = "relu")%>%
  layer_dense(units=1, activation="sigmoid")
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("acc")
)
history<-model%>%
  fit(
    x_train, y_train,
    epochs=20,
    batch_size=32,
    validation_data=list(x_val, y_val)
  )
#Tokenizing the data of the test set
imdb_dir<-"~/Downloads/acllmdb"
test_dir<-file.path(imdb_dir,"test")
labels<-c()
texts<-c()
for (label_type in c("neg","pos")){
  label<-switch(label_type, neg=0, pos=1)
  dir_name<-file.path(test_dir, label_type)
  for (fname in list.files(dir_name, pattern = glob2rx("*.txt"),
                           full.names = TRUE)){
    texts<-c(texts, readChar(fname, file.info(fname)$size))
    labels<-c(labels,label)
  }
}
sequences<-texts_to_sequences(tokenizer, texts)

#Pseudocode RNN
state_t=0
for (input_t, input_sequence){
  output_t<-f(input_t, state_t)
  state_t<-output_t
}

#More Detailed pseudocode for the RNN
state_t<-0
for (input_t in input_sequence){
  output_t<-activation(dot(W. input_t)+dot(U, state_t)+b)
  state_t<-output_t
}

#Implementation of a simple RNN
timesteps<-100
input_features<-32
output_features<-64
random_array<-function(dim){
  array(runif(prod(dim)), dim=dim)
}
inputs<-random_array(dim=c(timesteps, input_features))
state_t<-rep_len(0, length=c(output_features))
W<-random_array(dim=c(output_features, input_features))
U<-random_array(dim=c(output_features, output_features))
b<-random_array(dim=c(output_features,1))
output_sequence<-array(0, dim=c(timesteps, output_features))
for (i in 1:nrow(inputs)){
  input_t<-inputs[i,]
  output_t<-tanh(as.numeric((W %*% input_t)+(U %*% state_t)+b))
  output_sequence[i,]<-as.numeric(output_t)
  state_t<-output_t
}

#Using layer_simple_rnn and returns only the ouput at the last timestep
library(keras)
model<-keras_model_sequential()%>%
  layer_embedding(input_dim = 10000, output_dim = 32)%>%
  layer_simple_rnn(units=32)
summary(model)

#Using layer_sim_rnn and returns the full state sequence
model<-keras_model_sequential()%>%
  layer_embedding(input_dim = 10000, output_dim = 32)%>%
  layer_simple_rnn(units = 32, return_sequences = TRUE)
summary(model)

#Stack several recurrent layers one after the other in order to increase the representational power of a network.
model<-keras_model_sequential()%>%
  layer_embedding(input_dim = 10000, output_dim = 32)%>%
  layer_simple_rnn(units=32, return_sequences = TRUE)%>%
  layer_simple_rnn(units=32, return_sequences = TRUE)%>%
  layer_simple_rnn(units=32, return_sequences = TRUE)%>%
  layer_simple_rnn(units=32)
summary(model)

#Preparing the IMDB data
library(keras)
max_features<-10000
maxlen<-500
batch_size<-32
cat("Loading data...\n")
imdb<-dataset_imdb(num_words = max_features)
c(c(input_train, y_train), c(input_test, y_test))%<-%imdb
cat(length(input_train),"train sequences\n")
cat(length(input_test),"test sequences\n")
cat("Pad sequences (samples x time)\n")
input_train<-pad_sequences(input_train, maxlen = maxlen)
input_test<-pad_sequences(input_test, maxlen = maxlen)
cat("input_train shape:", dim(input_train),"\n")
cat("input_test shape:", dim(input_test),"\n")

#Training the model with the embedding and simple RNN layers
model<-keras_model_sequential()%>%
  layer_embedding(input_dim=max_features, output_dim = 32)%>%
  layer_simple_rnn(units=32)%>%
  layer_dense(unit=1, activation = "sigmoid")
model%>%compile(
  optimizer="rmsprop",
  loss="binary_crossentropy",
  metrics=c("acc")
)
history<-model%>%
  fit(
    input_train,y_train,
    epochs=10,
    batch_size=128,
    validation_split=0.2
  )
plot(history)

#Using the LSTM layer in Keras
model<-keras_model_sequential()%>%
  layer_embedding(input_dim = max_features, output_dim = 32)%>%
  layer_lstm(units=32)%>%
  layer_dense(units=1, activation="sigmoid")
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

#Inspecting the data of the Jena weather dataset
dir.create("~/Downloads/jena_climate", recursive = TRUE)
download.file("https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip","~/Downloads/jena_climate/jena_climate_2009_2016.csv.zip")
unzip( "~/Downloads/jena_climate/jena_climate_2009_2016.csv.zip", exdir = "~/Downloads/jena_climate"
)
library(tibble)
library(readr)
data_dir<-"~/Downloads/jena_climate"
fname<-file.path(data_dir, "jena_climate_2009_2016.csv")
data<-read_csv(fname)
glimpse(data)
#Plot the temperature over time
library(ggplot2)
ggplot(data, aes(x=1:nrow(data),y=data$`T (degC)`))+geom_line()
#Plotting the first 10 days of the temperature
ggplot(data[1:1440,], aes(x=1:1440, y=data$`T (degC)`))+geom_line()
#Sequence generator
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
mean<-apply(train_data,2,mean)
std<-apply(train_data,2,sd)
data<-scale(data, center = mean, scale = std)
#Generator yielding timeseries samples and their targets
generator<-function(data, lookback, delay, min_index, max_index, shuffle=FALSE, batch_size=128, step=6){
  if (is.null(max_index))max_index<-nrow(data)-delay-1
  i<-min_index+lookback
  function(){
    if(shuffle){
      rows<-sample(c((min_index+lookback):max_index),size=batch_size)
    } else{
      if (i+batch_size>=max_index)
        i<<-min_index+lookback
      rows<-c(i:min(i+batch_size,max_index))
      i<<-i+length(rows)
    }
    samples<-array(0, dim=c(length(rows),
                            lookback/step,
                            dim(data)[[-1]]))
    targets<-array(0, dim=c(length(rows)))
    for (j in 1:length(rows)){
      indices<-seq(rows[[j]]-lookback, rows[[j]],
                   length.out = dim(samples)[[2]])
      samples[j,,]<-data[indices,]
      targets[[j]]<-data[rows[[j]]+delay,2]
    }
    list(samples,targets)
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
  min_index = 1,
  max_index=200000,
  shuffle=TRUE,
  step=step,
  batch_size = batch_size
)
val_gen=generator(
  data,
  lookback=lookback,
  delay=delay,
  min_index=200001,
  max_index = 300000,
  step=step,
  batch_size = batch_size
)
test_gen<-generator(
  data,
  lookback=lookback,
  delay=delay,
  min_index=300001,
  max_index = NULL,
  step=step,
  batch_size = batch_size
)
val_steps<-(300000-200001-lookback)/batch_size
test_steps<-(nrow(data)-300001-lookback)/batch_size
#Computing the common-sense baseline MAE
evaluate_naive_method<-function(){
  batch_maes<-c()
  for (step in 1:val_steps){
    c(samples, targets)%<-% val_gen()
    preds<-samples[,dim(samples)[[2]],2]
    mae<-mean(abs(preds-targets))
    batch_maes<-c(batch_maes,mae)
  }
  print(mean(batch_maes))
}
evaluate_naive_method()
#Converting the MAE back to a Celsius error
celsius_mae<-0.29*std[[2]]

#Training and evaluating a densely connected model
library(keras)
model<-keras_model_sequential()%>%
  layer_flatten(input_shape=c(lookback/step, dim(data)[-1]))%>%
  layer_dense(units=32, activation = "relu")%>%
  layer_dense(units=1)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%
  fit_generator(
    train_gen,
    steps_per_epoch = 500,
    epochs=20,
    validation_data = val_gen,
    validation_steps = val_steps
  )
plot(history)

#Training and evaluating a model with layer_gru
model<-keras_model_sequential()%>%
  layer_gru(units=32, input_shape = list(NULL, dim(data)[[-1]]))%>%
  layer_dense(units=1)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%
  fit_generator(
    train_gen,
    steps_per_epoch = 100,
    epochs = 20,
    validation_data = val_gen,
    validation_steps = val_steps
  )
plot(history)

#Training and evaluating a dropout-regularized GRU-based model
model<-keras_model_sequential()%>%
  layer_gru(units=32, dropout = 0.2, recurrent_dropout = 0.2, input_shape=list(NULL, dim(data)[[-1]]))%>%
  layer_dense(units=1)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%
  fit_generator(
    train_gen,
    steps_per_epoch = 10,
    epochs = 40,
    validation_data = val_gen,
    validation_steps = val_steps
  )

#Training and evaluating a dropout-regularized, stacked GRU model
model<-keras_model_sequential()%>%
  layer_gru(units=32,
            dropout = 0.1,
            recurrent_dropout = 0.5,
            return_sequences = TRUE,
            input_shape = list(NULL, dim(data)[[-1]]))%>%
  layer_gru(units=64,
            activation = "relu",
            dropout = 0.1,
            recurrent_dropout = 0.5)%>%
  layer_dense(units=1)
model%>%
  compile(
    optimizer=optimizer_rmsprop(),
    loss="mae"
  )
history<-model%>%
  fit_generator(
    train_gen,
    steps_per_epoch = 10,
    epochs = 10,
    validation_data = val_gen,
    validation_steps = val_steps
  )

#Training and evaluating an LSTM using reversed sequences
library(keras)
max_features<-10000
maxlen<-500
imdb<-dataset_imdb(num_words = max_features)
c(c(x_train,y_train),c(x_test,y_test))%<-%imdb
x_train<-lapply(x_train, rev)
x_test<-lapply(x_test,rev)
x_train<-pad_sequences(x_train,maxlen = maxlen)
x_test<-pad_sequences(x_test, maxlen=maxlen)
model<-keras_model_sequential()%>%
  layer_embedding(input_dim = max_features,output_dim = 128)%>%
  layer_lstm(units=32)%>%
  layer_dense(units=1,activation = "sigmoid")
model%>%
  compile(
    optimizer="rmsprop",
    loss="binary_crossentropy",
    metrics=c("acc")
  )
history<-model%>%
  fit(
    x_train,y_train,
    epochs=10,
    batch_size=128,
    validation_split=0.2
  )

#Training and evaluating a bidirectional LSTM
model<-keras_model_sequential()%>%
  layer_embedding(input_dim=max_features,output_dim = 32)%>%
  bidirectional(
    layer_lstm(units = 32)
  )%>%
  layer_dense(units=1, activation = "sigmoid")
model%>%
  compile(
    optimizer="rmsprop",
    loss="binary_crossentropy",
    metrics=c("acc")
  )
history<-model%>%
  fit(
    x_train, y_train,
    epochs=10,
    batch_size=128,
    validation_split=0.2
  )

#Training a bidirectional GRU
model<-keras_model_sequential()%>%
  bidirectional(
    layer_gru(units=32),
    input_shape = list(NULL, dim(data)[[-1]])
  )%>%
  layer_dense(units=1)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%
  fit_generator(
    train_gen,
    steps_per_epoch = 500,
    epochs=40,
    validation_data = val_gen,
    validation_steps = val_steps
  )

#Implementing a 1D convnet
#Preparing the IMDB data
library(keras)
max_features<-10000
max_len<-500
cat("Loading data...\n")
imdb<-dataset_imdb(num_words = max_features)
c(c(x_train,y_train),c(x_test,y_test))%<-%imdb
cat(length(x_train),"train sequences\n")
cat(length(x_test),"test sequences")
cat("Pad sequences (samples x time)\n")
x_train<-pad_sequences(x_train, maxlen = max_len)
x_test<-pad_sequences(x_test, maxlen=max_len)
cat("x_train shape:",dim(x_train),"\n")
cat("x_test shape:",dim(x_test),"\n")
#Training and evaluating a simple 1D convnet on the IMDB data
model<-keras_model_sequential()%>%
  layer_embedding(input_dim = max_features,output_dim = 128,input_length = max_len)%>%
  layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu")%>%
  layer_max_pooling_1d(pool_size = 5)%>%
  layer_conv_1d(filters=32, kernel_size = 7, activation = "relu")%>%
  layer_global_max_pooling_1d()%>%
  layer_dense(units = 1)
summary(model)
model%>%compile(
  optimizer=optimizer_rmsprop(lr=1e-4),
  loss="binary_crossentropy",
  metrics=c("acc")
)
history<-model%>%
  fit(
    x_train,y_train,
    epochs=10,
    batch_size=128,
    validation_split=0.2
  )

#Combining CNNS and RNNs to process long sequences
#Training and evaluating a simple 1D convnet on the Jena data
model<-keras_model_sequential()%>%
  layer_conv_1d(filters = 32, kernel_size = 5,activation = "relu",
                input_shape=list(NULL, dim(data)[[-1]]))%>%
  layer_max_pooling_1d(pool_size = 3)%>%
  layer_conv_1d(filters=32, kernel_size = 5, activation = "relu")%>%
  layer_max_pooling_1d(pool_size = 3)%>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu")%>%
  layer_global_max_pooling_1d()%>%
  layer_dense(units=1)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%
  fit_generator(
    train_gen,
    steps_per_epoch = 500,
    epochs = 20,
    validation_data = val_gen,
    validation_steps = val_steps
  )

#Preparing higher-resolution data generators for the Jena dataset
step<-3
lookback<-720
delay<-144
train_gen<-generator(
  data,
  lookback = lookback,
  delay=delay,
  min_index=1,
  max_index = 200000,
  shuffle = TRUE,
  step=step
)
val_gen<-generator(
  data=data,
  lookback=lookback,
  delay=delay,
  min_index = 200001,
  max_index=300000,
  step=step
)
test_gen<-generator(
  data,
  lookback=lookback,
  delay=delay,
  min_index=300001,
  max_index=NULL,
  step=step
)
val_steps<-(300000-200001-lookback)/128
test_steps<-(nrow(data)-300001-lookback)/128
#Model combining a 1D convolutional base and a GRU layer
model<-keras_model_sequential()%>%
  layer_conv_1d(filters=32, kernel_size = 5, activation = "relu",
                input_shape=list(NULL, dim(data)[[-1]]))%>%
  layer_max_pooling_1d(pool_size = 3)%>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu")%>%
  layer_gru(units=32, dropout = 0.1, recurrent_dropout = 0.5)%>%
  layer_dense(units=1)
summary(model)
model%>%compile(
  optimizer=optimizer_rmsprop(),
  loss="mae"
)
history<-model%>%
  fit_generator(
    train_gen,
    steps_per_epoch = 500,
    epochs = 20,
    validation_data = val_gen,
    validation_steps = val_steps
  )
