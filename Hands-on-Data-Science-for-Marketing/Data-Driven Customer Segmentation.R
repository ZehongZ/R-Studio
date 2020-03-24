#Import libraries
library(readxl)
library(dplyr)
#Load data
df<-read_excel("Online Retail.xlsx", sheet="Online Retail")
#Data cleanup
df<-df[which(df$Quantity>0),]
#Dropping records with no customer ID
df<-na.omit(df)
#Excluding an incomplete month
df<-df[which(df$InvoiceDate<"2011-12-01"),]
#Computing total sales
df$Sales<-df$Quantity*df$UnitPrice
#Per-customer Data
customerDF<-df%>%
  group_by(CustomerID)%>%
  summarise(TotalSales=sum(Sales),
            OrderCount=length(unique(InvoiceDate)))%>%
  mutate(AvgOrderValue=TotalSales/OrderCount)
rankDF<-customerDF%>%
  mutate(TotalSales=rank(TotalSales), OrderCount=rank(OrderCount, ties.method="first"), AvgOrderValue=rank(AvgOrderValue))
normalizedDF<-rankDF%>%
  mutate(TotalSales=scale(TotalSales), OrderCount=scale(OrderCount), AvgOrderValue=scale(AvgOrderValue))
summary(normalizedDF)
sapply(normalizedDF,sd)
#K-means clustering
cluster<-kmeans(normalizedDF[c("TotalSales","OrderCount","AvgOrderValue")],4)
cluster$cluster
cluster$centers
#Cluster labels
normalizedDF$Cluster<-cluster$cluster
#Visualize the cluster
library(ggplot2)
ggplot(normalizedDF, aes(x=AvgOrderValue, y=OrderCount, color=Cluster))+geom_point()
#Selecting the best number of cluster
library(cluster)
for(n_cluster in 4:8){
  cluster<-kmeans(normalizedDF[c("TotalSales","OrderCount","AvgOrderValue")],n_cluster)
  silhouetteScore<-mean(
    silhouette(
      cluster$cluster,
      dist(normalizedDF[c("TotalSales","OrderCount","AvgOrderValue")], method="euclidean")
    )[,3]
  )
  print(sprintf('Silhouette Score for %i Cluster: %0.4f', n_cluster, silhouetteScore))
}
#Interpreting Customer Segments
cluster<-kmeans(normalizedDF[c("TotalSales","OrderCount","AvgOrderValue")],4)
normalizedDF$Cluster<-cluster$cluster
#Cluster centers
cluster$centers
#High Value cluster
highValueCustomers<-unlist(
  customerDF[which(normalizedDF$Cluster==2),'CustomerID'][,1],use.names = FALSE
)
df[which(df$CustomerID%in%highValueCustomers),]%>%
  group_by(Description)%>%
  summarise(Count=n())%>%
  arrange(desc(Count))
