#8.1
brand.ratings <- read.csv("http://goo.gl/IQl8nc")
head(brand.ratings)
str(brand.ratings)
tail(brand.ratings)
summary(brand.ratings)

#8.1.1
#Rescaling the data
brand.sc<-brand.ratings
brand.sc[,1:9]<-scale(brand.ratings[,1:9])
summary(brand.sc)
library(corrplot)
corrplot(cor(brand.sc[,1:9]), order="hclust")

#8.1.2
#Aggregate Mean Ratings by Brand
brand.mean<-aggregate(.~brand, data=brand.sc, mean)
brand.mean
rownames(brand.mean)<-brand.mean[,1]
brand.mean<-brand.mean[,-1]
brand.mean
#Heatmap
library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(brand.mean), col=brewer.pal(9, "GnBu"), trace="none",key=FALSE, dend="none",main="\n\n\n\n\nBrand attributes")

#8.2.1
#PCA Example
set.seed(98286)
xvar<-sample(1:10, 100, replace=TRUE)
yvar<-xvar
yvar[sample(1:length(yvar),50)]<-sample(1:10, 50, replace=TRUE)
zvar<-yvar
zvar[sample(1:length(zvar),50)]<-sample(1:10, 50, replace=TRUE)
my.vars<-cbind(xvar, yvar, zvar)
plot(yvar~xvar, data=jitter(my.vars))
cor(my.vars)
my.pca<-prcomp(my.vars)
summary(my.pca)
my.pca
cor(my.pca$x)

#8.2.2
#Visualizing PCA
biplot(my.pca)

#8.2.3
#PCA for Brand Ratings
brand.pc<-prcomp(brand.sc[,1:9])
summary(brand.pc)
plot(brand.pc, type="l")
biplot(brand.pc)
brand.mean
brand.mu.pc<-prcomp(brand.mean, scale=TRUE)
summary(brand.mu.pc)

#8.2.4
#Perceptual Map of the Brands
biplot(brand.mu.pc, main="Brand positioning", cex=c(1.5,1))

#8.3.2
#Finding an EFA Solutions
library(nFactors)
nScree(brand.sc[,1:9])
eigen(cor(brand.sc[,1:9]))#3 values are biiger than 1.0
factanal(brand.sc[,1:9], factors = 2)
factanal(brand.sc[,1:9], factors = 3)

#8.3.3 EFA Rotations
library(GPArotation)
brand.fa.ob<-factanal(brand.sc[,1:9], factors = 3, rotation = "oblimin")
brand.fa.ob
#Visualize item factor relationships
library(gplots)
library(RColorBrewer)
heatmap.2(brand.fa.ob$loadings,
          col=brewer.pal(9, "Greens"), trace="none", key=FALSE, dend="none",Colv = FALSE, cexCol = 1.2, 
          main="\n\n\n\n\nFactor loadings for brand adjectives")

#8.3.4
#Using Factor Scores for Brands
brand.fa.ob<-factanal(brand.sc[,1:9], factors=3, rotation="oblimin", scores="Bartlett")
brand.scores<-data.frame(brand.fa.ob$scores)
brand.scores$brand<-brand.sc$brand
head(brand.scores)
#Aggregate the individual scores by brand
brand.fa.mean<-aggregate(.~brand, data=brand.scores, mean)
rownames(brand.fa.mean)<-brand.fa.mean[,1]
brand.fa.mean<-brand.fa.mean[,-1]
names(brand.fa.mean)<-c("Leader","Value","Latest")
brand.fa.mean
heatmap.2(as.matrix(brand.fa.mean),
          col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none",
          cexCol = 1.2, main="\n\n\n\n\n\nMean factor score by brand")

#8.4
#Multidimensional Scaling
brand.dist<-dist(brand.mean)
brand.mds<-cmdscale(brand.dist)
brand.mds
plot(brand.mds, type="n")
text(brand.mds, rownames(brand.mds), cex=2)
brand.rank<-data.frame(lapply(brand.mean, function(x) ordered(rank(x))))
str(brand.rank)
library(cluster)
brand.dist.r<-daisy(brand.rank, metric="gower")
brand.mds.r<-isoMDS(brand.dist.r)
plot(brand.mds.r$points, type="n")
text(brand.mds.r$points, levels(brand.sc$brand), cex=2)
