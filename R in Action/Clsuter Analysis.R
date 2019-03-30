#Hierachical clustering: single linkage, complete linkage, average linkage, centroid and Ward's method
#Partitioning: K-means and Partitioning around medoids

#Calculating distances
library(flexclust)
data("nutrient")
head(nutrient,4)
d<-dist(nutrient)
as.matrix(d)[1:4,1:4]

#Average-linkage clustering of the nutrient data
library(flexclust)
data("nutrient")
row.names(nutrient)<- tolower(row.names(nutrient))
nutrient.scaled<-scale(nutrient)
nutrient.scaled
d<-dist(nutrient.scaled)
fit.average<-hclust(d, method="average")
plot(fit.average, hang=-1, cex=.8, main= "Average Linkage Clustering")

#Selecting the numbre of clusters
library(NbClust)
devAskNewPage(ask=TRUE)
nc<-NbClust(nutrient.scaled, distance = "euclidean", min.nc = 2, max.nc = 15, method="average")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")
#2,3,5 and 15 get the most votes

#Obtaining the final cluster solution
clusters<-cutree(fit.average, k=5)
table(clusters)

#Describes clusters
aggregate(nutrient, by=list(cluster=clusters),median)
aggregate(as.data.frame(nutrient.scaled), by=list(cluster=clusters),median)
plot(fit.average, hang=-1, cex=.8, main="Average Linkage Clustering\n5 Cluster Solution")
rect.hclust(fit.average,k=5)

#K-means clustering of wine data
library(rattle)
data(wine)
head(wine)
df<-scale(wine[-1])
library(NbClust)
set.seed(1234)
devAskNewPage(ask=TRUE)
nc<-NbClust(df, min.nc=2, max.nc = 15, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
set.seed(1234)
fit.km<-kmeans(df, 3, nstart = 25)
fit.km$size
fit.km$centers
##Describe clusters
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

#A cross-tabulation of Type and cluster memebership
ct.km<-table(wine$Type, fit.km$cluster)
ct.km

#Quantify the agreement between type and cluster using an adjusted Rand index
library(flexclust)
randIndex(ct.km)


#Partitioning around medoids for the wine data
library(cluster)
set.seed(1234)
fit.pam<-pam(wine[-1],k=3,stand = TRUE)
fit.pam$medoids
ct.pam<-table(wine$Type, fit.pam$clustering)
randIndex(ct.pam)

