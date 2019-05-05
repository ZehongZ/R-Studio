#Import libraries
library(tidyverse)
library(gutenbergr)

#Import dataset
titles<-c(
  "The War of the Worlds",
  "Pride and Prejudice"
)
books<-gutenberg_works(title %in% titles)%>%
  gutenberg_download(meta_fields = "title")%>%
  mutate(document=row_number())
books

#Transform text data into a tidy format
library(tidytext)
tidy_books<-books%>%
  unnest_tokens(word, text)%>%
  group_by(word)%>%
  filter(n()>10)%>%
  ungroup()
tidy_books

#Most frequent words
library(ggplot2)
tidy_books%>%
  count(title, word, sort=TRUE)%>%
  anti_join(get_stopwords())%>%
  group_by(title)%>%
  top_n(20)%>%
  ungroup()%>%
  ggplot(aes(reorder_within(word, n, title),n, fill=title))+
  scale_x_reordered()+
  coord_flip()+
  facet_wrap(~title, scales="free")+
  scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, y="Word count",
       title="Most frequent words after removing stop words",
       subtitle = "Words like 'said' occupy similar ranks but other words are quite different")

#Building a machine learning model
library(resample)
library(rsample)
books_split<-books%>%
  select(document)%>%
  initial_split()
train_data<-training(books_split)
test_data<-testing(books_split)

#Transform tidy data structure to a sparse matrix 
sparse_words<-tidy_books%>%
  count(document, word)%>%
  inner_join(train_data)%>%
  cast_sparse(document,word,n)
class(sparse_words)
dim(sparse_words)
word_rownames<-as.integer(rownames(sparse_words))
books_joined<-data_frame(document=word_rownames)%>%
  left_join(books%>%
              select(document, title))

#Train classification model
library(glmnet)#Fit a logistic regression with LASSO regularization
library(doMC)
registerDoMC(cores=8)
is_jane<-books_joined$title=="Pride and Prejudice"
model<-cv.glmnet(sparse_words, is_jane,family="binomial",parallel = TRUE, keep=TRUE)
plot(model)
plot(model$glmnet.fit)

#Understanding and evaluating our model
##Using boom to check out the coefficients of the model
library(broom)
coefs<-model$glmnet.fit%>%
  tidy()%>%
  filter(lambda==model$lambda.1se)
##Which coefficients are the largest in size, in each direction
coefs%>%
  group_by(estimate>0)%>%
  top_n(10,abs(estimate))%>%
  ungroup()%>%
  ggplot(aes(fct_reorder(term, estimate),estimate,fill=estimate>0))+
  geom_col(alpha=0.8, show.legend = FALSE)+
  coord_flip()+
  labs(x=NULL,
       title = "Coefficients that increase/decrease probability the most",
       subtitle = "A document mentioning Martians is Unlikely to be written by Jane Austen")

#Test on test data
intercept<-coefs%>%
  filter(term=="(Intercept)")%>%
  pull(estimate)
classifications<-tidy_books%>%
  inner_join(test_data)%>%
  inner_join(coefs, by=c("word"="term"))%>%
  group_by(document)%>%
  summarize(score=sum(estimate))%>%
  mutate(probability=plogis(intercept+score))
classifications
library(yardstick)#TO build ROC curve
comment_classes<-classifications%>%
  left_join(books%>%
              select(title, document),by="document")%>%
  mutate(title=as.factor(title))
comment_classes%>%
  roc_curve(title, probability)%>%
  ggplot(aes(x=1-specificity, y=sensitivity))+
  geom_line(
    color="midnightblue",
    size=1.5
  )+
  geom_abline(
    lty=2,alpha=0.5,
    color="gray50",
    size=1.2
  )+
  labs(
    title="ROC curve for text classification using regularized regression",
    subtitle = "Predicting whether text was written by Jane Austen or H.G.Wells"
  )
comment_classes%>%
  roc_auc(title, probability)
#Confusion matrix
comment_classes%>%
  mutate(
    prediction=case_when(
      probability>0.5~"Pride and Prejudice",
      TRUE~"The War of the Worlds"
    ),
    prediction=as.factor(prediction)
  )%>%
  conf_mat(title,prediction)
comment_classes%>%
  filter(
    probability>.08,
    title=="The War of the Worlds"
  )%>%
  sample_n(10)%>%
  inner_join(books %>%
               select(document, text))%>%
  select(probability,text)
#Which documents here were incorrectly predicted to not be written by Jane Austen
comment_classes%>%
  filter(
    probability<.3,
    title=="Pride and Prejudice"
  )%>%
  sample_n(10)%>%
  inner_join(books %>%
               select(document, text))%>%
  select(probability,text)
