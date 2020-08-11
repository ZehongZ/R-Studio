#Building Gaussian Mixture model for clusterin
library(mclust)
library(tidyverse)
data(banknote, package = "mclust")
swissTib<-select(banknote, -Status)%>%
  as_tibble()
swissTib
swissScaled<-swissTib%>%scale()

#Performing and plotting mixture model clustering
swissMclust<-Mclust(swissTib)
plot(swissMclust)

