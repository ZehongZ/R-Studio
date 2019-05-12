####Concept####
#Estimated likelihood of an event or a potential outcome should be based on the evidence at hand across multiple trials, or opportunities for the event to occur

####Probability####
#Probability=observed data by dividing the number of trials in which the event occurred by the total number of trials

####The Naive Bayes algorithm####
#Relies on assumption of equally important and independent features
#Not ideal for datasets with many numeric features
#Estimated probabilities are less reliable than the predicted classes
#Numeric features should probably be binned.

####Filtering mobile phone spam with the Naive Bayes algorithm
sms_raw<-read.csv("sms_spam.csv",stringsAsFactors = FALSE)
str(sms_raw)
sms_raw$type<-factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
#cleaning and standardizing text data
library(tm)
sms_corpus<-VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2],as.character)
sms_corpus_clean<-tm_map(sms_corpus,content_transformer(tolower))
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)
sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords,stopwords())
sms_corpus_clean<-tm_map(sms_corpus_clean,removePunctuation)
removePunctuation("hello...world")
library(SnowballC)
wordStem(c("learn","learned","learning","learns"))
sms_corpus_clean<-tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)
#Splitting text documents into words
sms_dtm<-DocumentTermMatrix(sms_corpus_clean)
sms_dtm2<-DocumentTermMatrix(sms_corpus,control = list(
  tolower=TRUE,
  removeNumbers=TRUE,
  stopwords=TRUE,
  removePunctuation=TRUE,
  stemming=TRUE
))
sms_dtm
sms_dtm2
#Creating training and test datasets
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]
sms_train_labels<-sms_raw[1:4169,]$type
sms_test_labels<-sms_raw[4170:5559,]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
#Visualizing text data
library(wordcloud)
wordcloud(sms_corpus_clean,min.freq = 50,random.order = FALSE)
#Creating indicator features for frequent words
findFreqTerms(sms_dtm_train,5)
sms_freq_words<-findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)
sms_dtm_freq_train<-sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]
#Convert counts to Yes/No strings
convert_counts<-function(x){
  x<-ifelse(x>0, "Yes","No")
}
sms_train<-apply(sms_dtm_freq_train,MARGIN = 2,convert_counts)
sms_test<-apply(sms_dtm_freq_test,MARGIN = 2,convert_counts)
#Train a model on the data
library(e1071)
sms_classifier<-naiveBayes(sms_train,sms_train_labels)
#Evaluating model performance
sms_test_pred<-predict(sms_classifier,sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,prop.chisq = FALSE,prop.t = FALSE,dnn=c('predicted','actual'))
