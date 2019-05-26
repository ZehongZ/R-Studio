#Simple hold-out validation split
indices<-sample(1:nrow(data),size=0.8*nrow(data))
testing_data<-data[-indices,]
training_data<-data[indices,]

#K-fold cross-validation
k<-4
indices<-sample(1:nrow(data))
folds<-cut(indices, breaks=k, labels = FALSE)
validation_scores<-c()
for (i in 1:k){
  validation_indices<-which(folds--i, arr.ind=TRUE)
  validation_data<-data[validation_indices,]
  training_Data<-data[-validation_indices,]
  model<-get_model()
  model%>%train(train_data)
  results<-model%>%evaluate(validation_data)
  validataion_scores<-c(validation_scores, results$accuracy)
}
validation_score<-mean(validation_scores)
model<-get_model()
model%>%train(data)
results<-model%>%evaluate(test_data)
