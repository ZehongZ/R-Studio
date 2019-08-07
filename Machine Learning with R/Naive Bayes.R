#Import dataste
sms_raw<-read.csv("sms_spam.csv", stringsAsFactors = FALSE)

#Overview of the dataset
str(sms_raw)

#Transform class into factor variable
sms_raw$type<-factor(sms_raw$type)
str(sms_raw$type)

#See the distribution of the class
table(sms_raw$type)
prop.table(table(sms_raw$type))

#Remove numbers and punctuations; handle nosense words; break apart sentences into words
#Using VCorpus transform into a collection of text documents
#VectorSource() to create a source object 
library(tm)
sms_corpus<-VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)#Essentially it's a list
inspect(sms_corpus[1:2])#Look at the first and second messages

#To view one message
as.character(sms_corpus[[1]])

#To view the first two messages
lapply(sms_corpus[1:2], as.character)

#To transform text
sms_corpus_clean<-tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

#Remove numbers
sms_corpus_clean<-tm_map(sms_corpus_clean, removeNumbers)

#Remove stopwords
sms_corpus_clean<-tm_map(sms_corpus_clean, removeWords, stopwords())

#Remove Punctuation
sms_corpus_clean<-tm_map(sms_corpus_clean, removePunctuation)

#Transform all the words back to base form
library(SnowballC)
sms_corpus_clean<-tm_map(sms_corpus_clean, stemDocument)

#Remove blank spaces
sms_corpus_clean<-tm_map(sms_corpus_clean, stripWhitespace)

#Splitting text documents into words
sms_dtm<-DocumentTermMatrix(sms_corpus_clean)
sms_dtm
#Create DTM directly from the raw
sms_dtm2<-DocumentTermMatrix(sms_corpus, control = list(tolower=TRUE,
                                                        removeNumbers=TRUE,
                                                        removePunctuation=TRUE,
                                                        stemming=TRUE))

#Create training and test datasets
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]

#Creating labels
sms_train_labels<-sms_raw[1:4169,]$type
sms_test_labels<-sms_raw[4170:5559,]$type

#Confirm the subsets are representatitive 
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#Visualizing text data
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

#Comparing ham and spam
spam<-subset(sms_raw, type=='spam')
ham<-subset(sms_raw, type=='ham')
wordcloud(spam$text, max.words = 40, scale=c(3,0.5))
wordcloud(ham$text, max.words = 40, scale=c(3,0.5))

#Creating indicator features for frequent words
findFreqTerms(sms_dtm_train,5)#Occur at least 5 times
sms_freq_words<-findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)

sms_dtm_freq_train<-sms_dtm_train[, sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]

sms_dtm_freq_train[[1]]

#Convert numerical to categorical data
convert_counts<-function(x){
  x<-ifelse(x>0, "Yes","No")
}
sms_train<-apply(sms_dtm_freq_train, MARGIN=2, convert_counts)
sms_test<-apply(sms_dtm_freq_test, MARGIN=2, convert_counts)

#Training a model on the data
library(e1071)
sms_classifier<-naiveBayes(sms_train, sms_train_labels)

#Evaluating model performance
sms_test_pred<-predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,prop.chisq = FALSE, prop.t = FALSE, dnn=c("predicted","actual"))

#Improving model performance
sms_classifier2<-naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2<-predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq = FALSE, prop.t=FALSE, prop.r = FALSE, dnn=c("predicted","actual"))
