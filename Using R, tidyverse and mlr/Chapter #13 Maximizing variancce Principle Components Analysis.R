#Component analysis
#Loading and exploring the banknote dataset
library(tidyverse)
data(banknote, package = "mclust")
swissTib<-as_tibble(banknote)
swissTib

#Plotting the data with ggpairs
library(GGally)
ggpairs(swissTib, mapping=aes(col=Status))+theme_bw()

#Performing the PCA
pca<-select(swissTib, -Status)%>%
  prcomp(center=TRUE, scale=TRUE)
pca
summary(pca)

#Calcualting variable loadings
map_dfc(1:6, ~pca$rotation[,.]*sqrt(pca$sdev^2)[.])

#Plotting the PCA results
library(factoextra)
pcaDat<-get_pca(pca)
fviz_pca_biplot(pca, label="var")
fviz_pca_var(pca)
fviz_screeplot(pca, addlabels=TRUE, choice="eigenvalue")
fviz_screeplot(pca, addlabels=TRUE, choice="variance")

#Mapping the genuine and counterfeit labels onto PC1 and PC2
swissPCA<-swissTib%>%
  mutate(PCA1=pca$x[,1], PCA2=pca$x[,2])
ggplot(swissPCA, aes(PCA1, PCA2, col=Status))+
  geom_point()+
  theme_bw()

#Mapping the genuine and counterfeit labels onto PC1 and PC2
newBanknotes<-tibble(
  Length=c(214,216),
  Left=c(130,216),
  Right=c(132,129),
  Bottom=c(12,7),
  Top=c(12,8),
  Diagonal=c(138,142)
)
predict(pca, newBanknotes)
