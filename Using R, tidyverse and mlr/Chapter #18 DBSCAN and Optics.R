#Loading the tidyverse packages and exploring the dataset
library(tidyverse)
data(banknote, package = "mclust")
swissTib<-select(banknote, -Status)%>%
  as_tibble()
swissTib
swissScaled<-swissTib%>%scale()

#Plotting the data
library(GGally)
ggpairs(swissTab, upper = list(continuous="density"))+theme_bw()

#Plotting the k-NN distance plot
library(dbscan)
kNNdistplot(swissScaled, k=5)

#Defining our hyperparameter search space
dbsParamSpace<-expand.grid(eps=seq(1.2,2.0,0.1),
                           minPts=seq(1,9,1))

#Running DBSCAN on each combination of hyperparameters
swissDbs<- pmap(dbsParamSpace, dbscan, x=swissScaled)
swissDbs[[5]]

#Creating a tibble of cluster memberhsips from each DBSCAN permutation
clusterResults<-map_dfc(swissDbs, ~.$cluster)
clusterResults

#Using bind_cols() to bind the cluster memberships to the original data
swissClusters<-bind_cols(swissTib, clusterResults)
swissClusters

#Gathering the data ready for plotting
swissClustersGathered<-gather(swissClusters,
                              key="Permutation", value="Cluster",
                              -Length, -Left, -Right,
                              -Bottom, -Top, -Diagonal)
swissClustersGathered

#Plotting the cluster memberships of each permutation
ggplot(swissClustersGathered, aes(Right, Diagonal, col=as.factor(Cluster)))+
  facet_wrap(~ Permutation)+
  geom_point()+
  theme_bw()+
  theme(legend.position = "none")

#Defining the cluster_metrics() function
cluster_metrics<-function(data, clusters, dist_matric){
  list(db=clusterSim::index.DB(data, clusters)$DB,
       G1=clusterSim::index.G1(data, clusters),
       dunn=clValid::dunn(dist_matrix, clusters),
       clusters=length(unique(clusters)))
}

#Creating bootstrap samples
swissBoot<-map(1:10, ~{
  swissScaled%>%
    as_tibble()%>%
    sample_n(size=nrow(.), replace=TRUE)
})

#Performing the tuning experiment
metricsTib<-map_df(swissBoot, function(boot){
  clusterResult<-pmap(dbsParamSpace, dbscan, x=boot)
  
  map_df(clusterResult, function(permutation){
    clust<- as_tibble(permutation$cluster)
    filteredData<-bind_cols(boot, clust)%>%
      filter(value !=0)
    
    d<-dist(select(filteredData, -value))
    
    cluster_metrics(select(filteredData, -value),
                    clusters=filteredData$value, dist_matrix = d)
  })
})
#...come back next time

#First OPTICS model
#Ordering cases with OPTICS and extracting clusters
swissOptics<-optics(swissScaled, minPts=9)
plot(swissOptics)
swissOpticsXi<- extractXi(swissOptics, xi=0.05)

#Plotting the OPTICS clusters
swissTib%>%
  mutate(cluster=factor(swissOpticsXi$cluster))%>%
  ggpairs(mapping = aes(col=cluster),
          upper=list(continuous="points"))+
  theme_bw()
