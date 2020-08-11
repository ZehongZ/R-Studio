#Build our first agglomerative hierarchical clustering model
#Load packages
library(mlr)
library(tidyverse)

#Load dataset
data(GvHD, package = "mclust")
gvhdTib<-as.tibble(GvHD.control)
gvhdTib
gvhdScaled<-gvhdTib%>%scale()

gvhdDist<-dist(gvhdScaled, method="euclidean")
gvhdHclust<-hclust(gvhdDist, method="ward.D2")

#Plotting the dendrogram
gvhdDend<- as.dendrogram(gvhdHclust)
plot(gvhdDend, leaflab = "none")

#Defining the cluster_metrics function
cluster_metrics<-function(data, clusters, dist_matrix){
  list(db = clusterSim::index.DB(data, clusters)$DB,
       G1 = clusterSim::index.G1(data, clusters),
       dunn = clValid::dunn(dist_matrix, clusters),
       clusters=length(unique(clusters)))
}

#Creating bootstrap samples
gvhdBoot<-map(1:10, ~{
  gvhdScaled%>%
    as_tibble()%>%
    sample_n(size=nrow(.), replace=TRUE)
})

#Calculating performance metrices of our clustering model
library(clValid)
library(cluster)
metricsTib<-map_df(gvhdBoot, function(boot){
  d<-dist(boot, method="euclidean")
  cl <- hclust(d, method="ward.D2")
  
  map_df(3:8, function(k){
    cut<-cutree(cl, k=k)
    cluster_metrics(boot, clusters=cut, dist_matrix = d)
  })
})

#Transforming the data ready for plotting
metricsTib<-metricsTib%>%
  mutate(bootstrap=factor(rep(1:10, each=6)))%>%
  gather(key="Metric", value="Value", -clusters, -bootstrap)

#Calculating metrics
ggplot(metricsTib, aes(as.factor(clusters), Value))+
  facet_wrap(~Metric, scales = "free_y")+
  geom_line(size=0.1, aes(group=bootstrap))+
  geom_line(stat="summary", fun.y="mean", aes(group=1))+
  stat_summary(fun.data="mean_cl_boot",
               geom="crossbar", width=0.5, fill="white")+
  theme_bw()

#Cutting the tree
gvhdCut<-cutree(gvhdHclust, k=4)
plot(gvhdDend, leaflab="none")
rect.hclust(gvhdHclust, k=4)

#Plotting the clusters
library(GGally)
gvhdTib<-mutate(gvhdTib, hclustCluster=as.factor(gvhdCut))
ggpairs(gvhdTib, aes(col=hclustCluster),
        upper=list(continuours="density"),
        lower=list(continuous=wrap("points", size=0.5)))+
  theme_bw()

#Calculate Jaccard's distance
library(fpc)
par(mfrow=c(3,4))
clustBoot<-clusterboot(gvhdDist, B=10,
                       clustermethod=disthclustCBI,
                       k=4, cut="number", method="ward.D2",
                       showplots=TRUE)
clustBoot