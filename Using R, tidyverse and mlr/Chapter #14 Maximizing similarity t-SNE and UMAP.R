#Performing t-SNE

#Installing Packages
library(Rtsne)
library(tidyverse)

#Loading and exploring the banknote dataset
data(banknote, package = "mclust")
swissTib<-as_tibble(banknote)
swissTib

#Running t-SNE
swissTsne<-select(swissTib, -Status)%>%
  Rtsne(perplexiy=30, theta=0, max_iter = 5000, verbose = TRUE)

#Plotting the t-SNE embedding
swissTibTsne<-swissTib%>%
  mutate_if(.funs=scale, .predicate = is.numeric, scale=FALSE)%>%
  mutate(tSNE1=swissTsne$Y[,1], tSNE2=swissTsne$Y[,2])%>%
  gather(key="Variable", value = "Value", c(-tSNE1, -tSNE2,-Status))
ggplot(swissTibTsne, aes(tSNE1, tSNE2, col=Value, shape=Status))+
  facet_wrap(~ Variable)+
  geom_point(size=3)+
  scale_color_gradient(low="dark blue", high="cyan")+
  theme_bw()


#Building UMAP
#Load packages
library(umap)
#Performing UMAP
swissUmap<- select(swissTib, -Status)%>%
  as.matrix()%>%
  umap(n_neighbors=7, min_dist=0.1,
       metric="manhattan", n_epochs=200, verbose=TRUE)

#Plotting the UMAP embedding
swissTibUmap<-swissTib%>%
  mutate_if(.funs=scale, .predicate = is.numeric, scale=FALSE)%>%
  mutate(UMAP1=swissUmap$layout[,1], UMAP2=swissUmap$layout[,2])%>%
  gather(key="Variable", value = "Value", c(-UMAP1, -UMAP2, -Status))
ggplot(swissTibUmap, aes(UMAP1, UMAP2, col=Value, shape=Status))+
  facet_wrap(~ Variable)+
  geom_point(size=3)+
  scale_color_gradient(low="dark blue", high="cyan")+
  theme_bw()

#Computing the UMAP embeddings of new data
newBanknotes<-tibble(
  Length=c(214,216),
  Left=c(130,216),
  Right=c(132,129),
  Bottom=c(12,7),
  Top=c(12,8),
  Diagonal=c(138,142)
)
predict(swissUmap, newBanknotes)
