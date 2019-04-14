#Tidying DocumentTermMatrix Objects
library(tm)
library(topicmodels)
data("AssociatedPress")
AssociatedPress
terms<-Terms(AssociatedPress)
head(terms)

#Turn into a data frame with one token per document per row
library(dplyr)
library(tidytext)
ap_td<-tidy(AssociatedPress)
ap_td

#Sentiment analysis 
ap_sentiments<-ap_td%>%
  inner_join(get_sentiments("bing"),by=c(term="word"))
ap_sentiments

#Visualize most often contributed to positive or negative sentiment
library(ggplot2)
ap_sentiments%>%
  count(sentiment,term,wt=count)%>%
  ungroup()%>%
  filter(n>200)%>%
  mutate(n=ifelse(sentiment=="negative",-n,n))%>%
  mutate(term=reorder(term,n))%>%
  ggplot(aes(term, n, fill=sentiment))+
  geom_bar(stat="identity")+
  ylab("Contribution to sentiment")+
  coord_flip()

#Tidying dfm objects
library(methods)
library(quanteda)
data("data_corpus_inaugural")
inaug_dfm<-quanteda::dfm(data_corpus_inaugural,verbose=FALSE)
inaug_dfm
inaug_td<-tidy(inaug_dfm)
inaug_td
#Find the words most specific to each of the inaugural speeches
inaug_tf_idf<-inaug_td%>%
  bind_tf_idf(term, document, count)%>%
  arrange(desc(tf_idf))
inaug_tf_idf

#Extract the year from each document's name and compute the total number of words within each year
library(tidyr)
year_term_counts<-inaug_td%>%
  extract(document, "year","(\\d+)", convert=TRUE)%>%
  complete(year, term, fill=list(count=0))%>%
  group_by(year)%>%
  mutate(year_total=sum(count))

year_term_counts%>%
  filter(term %in% c("god","america","foreign","union","constitution","freedom"))%>%
  ggplot(aes(year, count/year_total))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~term, scales = "free_y")+
  scale_y_continuous(labels=scales::percent_format())+
  ylab("% frequency of word in inaugural address")

#Casting Tidy Text Data into a Matrix
ap_td%>%
  cast_dtm(document, term, count)
ap_td%>%
  cast_dfm(term, document,count)
library(Matrix)
m<-ap_td%>%
  cast_sparse(document, term, count)
class(m)
dim(m)

library(janeaustenr)
austen_dtm<-austen_books()%>%
  unnest_tokens(word, text)%>%
  count(book, word)%>%
  cast_dtm(book, word, n)
austen_dtm

#Tidying Corpus Objects with Metadata
data(acq)
acq
acq[1]
acq_td<-tidy(acq)
acq_td
acq_tokens<-acq_td%>%
  select(-places)%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words, by="word")
#Most common words
acq_tokens%>%
  count(word, sort=TRUE)
acq_tokens%>%
  count(id,word)%>%
  bind_tf_idf(word, id, n)%>%
  arrange(desc(tf_idf))

#Mining Financial Articles
library(tm.plugin.webmining)
library(purrr)
company<-c("Microsoft","Apple","Google","Amazon","Facebook","Twitter","IBM","Yahoo","Netflix")
symbol<-c("MSFT","AAPL","GOOG","AMAZN","FB","TWTR","IBM","YHOO","NFLX")
download_articles<-function(symbol){
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:",symbol)))
}
stock_articles<-data_frame(company=company,symbo=symbol)%>%
  mutate(corpus=purrr::map(symbol, download_articles))
