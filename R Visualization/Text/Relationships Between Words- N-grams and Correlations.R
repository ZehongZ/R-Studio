#Tokenizing by N-gram
library(dplyr)
library(tidytext)
library(janeaustenr)
austen_bigrams<-austen_books()%>%
  unnest_tokens(bigram, text, token="ngrams",n=2)
austen_bigrams

#Counting the Filtering N-grams
austen_bigrams%>%
  count(bigram,sort=TRUE)

#Seperate columns and remove stop words
library(tidyr)
bigrams_separated<-austen_bigrams%>%
  separate(bigram, c("word1","word2",sep=" "))

bigrams_filtered<-bigrams_separated%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)

#New bigram counts
bigram_counts<-bigrams_filtered%>%
  count(word1, word2, sort=TRUE)
bigram_counts

#Recombine the columns
bigrams_united<-bigrams_filtered%>%
  unite(bigram, word1, word2, sep=" ")
bigrams_united

#Most common trigrams, which are consecutive sequences of three words
austen_books()%>%
  unnest_tokens(trigram, text, token = "ngrams",n=3)%>%
  separate(trigram, c("word1", "word2","word3"),sep=" ")%>%
  filter(!word1 %in% stop_words$word,
         !word2%in% stop_words$word,
         !word3%in% stop_words$word)%>%
  count(word1, word2, word3,sort=TRUE)

#Analyzing Bigrams
bigrams_filtered%>%
  filter(word2=="street")%>%
  count(book, word1, sort=TRUE)

#Look at the tf-idf of bigrams across Austen novels
bigram_tf_idf<-bigrams_united%>%
  count(book, bigram)%>%
  bind_tf_idf(bigram, book, n)%>%
  arrange(desc(tf_idf))
bigram_tf_idf

#Using Bigrams to Provide Context in Sentiment Analysis
bigrams_separated%>%
  filter(word1=="not")%>%
  count(word1, word2, sort=TRUE)

AFINN<-get_sentiments("afinn")
AFINN

not_words<-bigrams_separated%>%
  filter(word1=="not")%>%
  inner_join(AFINN, by=c(word2="word"))%>%
  count(word2, score, sort=TRUE)%>%
  ungroup()
not_words

#Find which words contributed the most in the "wrong" direction
not_words%>%
  mutate(contribution=n*score)%>%
  arrange(desc(abs(contribution)))%>%
  head(20)%>%
  mutate(word2=reorder(word2, contribution))%>%
  ggplot(aes(word2,n*score, fill=n*score>0))+
  geom_col(show.legend = FALSE)+
  xlab("Words preceded by \"not\"")+
  ylab("Sentiment score * number of occurrences")+
  coord_flip()

#Pick 4 common words that negate the subsequent term and examine all of them at once
negation_words<-c("not","no","never","without")
negated_words<-bigrams_separated%>%
  filter(word1%in%negation_words)%>%
  inner_join(AFINN, by=c(word2="word"))%>%
  count(word1, word2, score, sort=TRUE)%>%
  ungroup()

#Visualizing a Network of Bigrams with ggraph
library(igraph)
bigram_counts
bigram_graph<-bigram_counts%>%
  filter(n>20)%>%
  graph_from_data_frame()#Filter for only relatively common combination
bigram_graph

library(ggraph)
set.seed(2017)
ggraph(bigram_graph, layout="fr")+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name),vjust=1,hjust=1)

set.seed(2016)
a<-grid::arrow(type="closed", length = unit(.15,"inches"))
ggraph(bigram_graph,layout = "fr")+
  geom_edge_link(aes(edge_alpha=n),show.legend = FALSE, arrow = a, end_cap=circle(.07, 'inches'))+
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name),vjust=1,hjust=1)+
  theme_void()

#Visualizing Bigrams in Other Texts
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
count_bigrams<-function(dataset){
  dataset%>%
    unnest_tokens(bigram, text, token = "ngrams",n=2)%>%
    separate(bigram, c("word1","word2"),sep=" ")%>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word)%>%
    count(word1,word2,sort=TRUE)
}

visualize_bigrams<-function(bigrams){
  set.seed(2016)
  a<-grid::arrow(type="closed",length = unit(.15,"inches"))
  bigrams%>%
    graph_from_data_frame()%>%
    ggraph(layout="fr")+
    geom_edge_link(aes(edge_alpha=n),show.legend = FALSE, arrow=a)+
    geom_node_point(color="lightblue",size=5)+
    geom_node_text(aes(label=name),vjust=1,hjust=1)+
    theme_void()
}

#The King James version is book 10 on Project Genberg
library(gutenbergr)
kjv<-gutenberg_download(10)
library(stringr)
kjv_bigrams<-kjv%>%
  count_bigrams()

#Filter out rate combinations
kjv_bigrams%>%
  filter(n>40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d"),)%>%
  visualize_bigrams()

#Counting and Correlating among Sections
austen_section_words<-austen_books()%>%
  filter(book=="Pride & Prejudice")%>%
  mutate(section=row_number() %/% 10)%>%
  filter(section>0)%>%
  unnest_tokens(word, text)%>%
  filter(!word %in% stop_words$word)
austen_section_words

library(widyr)
word_pairs<-austen_section_words%>%
  pairwise_count(word, section, sort=TRUE)
word_pairs

word_pairs%>%
  filter(item1=="darcy")

#Examining Pairwise Correlation
word_cors<-austen_section_words%>%
  group_by(word)%>%
  filter(n()>=20)%>%
  pairwise_cor(word, section, sort=TRUE)
word_cors

word_cors%>%
  filter(item1=="pounds")
word_cors%>%
  filter(item1 %in% c("elizabeth","pounds","married","pride"))%>%
  group_by(item1)%>%
  top_n(6)%>%
  ungroup()%>%
  mutate(item2=reorder(item2,correlation))%>%
  ggplot(aes(item2, correlation))+
  geom_bar(stat='identity')+
  facet_wrap(~item1, scales="free")+
  coord_flip()

set.seed(2016)
word_cors%>%
  filter(correlation>.15)%>%
  graph_from_data_frame()%>%
  ggraph(layout="fr")+
  geom_edge_link(aes(edge_alpha=correlation),show.legend = FALSE)+
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name),repel = TRUE)+
  theme_void()
