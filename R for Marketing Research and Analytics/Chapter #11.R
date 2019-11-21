#11.2
#Segmentation Data
seg.raw <- read.csv("http://goo.gl/qw303p")
#Overview of the data
summary(seg.raw)
str(seg.raw)
#Remove the known segment assignments
seg.df<-seg.raw[,-7]
#Overview of the data
summary(seg.df)

#11.3.1.1
#A quick check function
seg.summ<-function(data, groups){
  aggregate(data,list(groups), function(x) mean(as.numeric(x)))
}
seg.sum(seg.df, seg.raw$Segment)

#11.3.2
#Hierarchical Clustering: hclust() Basics
#Vector of difference
c(1,2,3)-c(2,3,2)
sum((c(1,2,3)-c(2,3,2))^2)
sqrt(sum((c(1,2,3)-c(2,3,2))^2))
dist(rbind(c(1,2,3),c(2,3,2)))
d<-dist(seg.df[,c("age","income","kids")])
as.matrix(d)[1:5,1:5]
library(cluster)
seg.dist<-daisy(seg.df)
as.matrix(seg.dist)[1:5,1:5]
seg.hc<-hclust(seg.dist, method="complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]])
seg.df[c(101,107),]
seg.df[c(278,294),]
seg.df[c(173,141),]
cor(cophenetic(seg.hc),seg.dist)

#11.3.3
#Hierarchical Clustering Continued: Groups from hclust()
plot(seg.hc)
rect.hclust(seg.hc, k=4, border="red")
seg.hc.segment<-cutree(seg.hc, k=4)
table(seg.hc.segment)
plot(jitter(as.numeric(seg.df$gender))~
       jitter(as.numeric(seg.df$subscribe)),
     col=seg.hc.segment, yaxt="n", xaxt="n", ylab="",xlab="")
axis(1, at=c(1,2), labels=c("Subscribe:No", "Subscribe: Yes"))
axis(2, at=c(1,2), labels=levels(seg.df$gender))

#11.3.4
#Mean-Based Clustering
seg.df.num<-seg.df
seg.df.num$gender<-ifelse(seg.df$gender=="Male",0,1)
seg.df.num$ownHome<-ifelse(seg.df$ownHome=="ownNo",0,1)
seg.df.num$subscribe<-ifelse(seg.df$subscribe=="subNo",0,1)
summary(seg.df.num)
set.seed(96743)
seg.k<-kmeans(seg.df.num,centers=4)
seg.summ(seg.df, seg.k$cluster)
boxplot(seg.df.num$income~seg.k$cluster, ylab="Income",xlab="Cluster")
library(cluster)
clusplot(seg.df, seg.k$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, main="K-means Cluster plot")
seg.mc4<-Mclust(seg.df.num,G=4)
summary(seg.mc4)

#11.3.6
#Comparing Models with BIC()
BIC(seg.mc, seg.mc4)
seg.summ(seg.df, seg.mc$classification)
library(cluster)
clusplot(seg.df, seg.mc$classification, color=TRUE, shade = TRUE,
         labels=4, lines=0, main = "Model-based cluster plot")

#11.3.7
#Latent Class Analysis: poLCA()
#Transform Numeric to categorical
seg.df.cut<-seg.df
seg.df.cut$age<-factor(ifelse(seg.df$age<median(seg.df$age),1,2))
seg.df.cut$income<-factor(ifelse(seg.df$income<median(seg.df$income),1,2))
seg.df.cut$kids<-factor(ifelse(seg.df$kids<median(seg.df$kids),1,2))
summary(seg.df.cut)
seg.f<-with(seg.df.cut,
            cbind(age, gender, income, kids, ownHome, subscribe)~1)
summary(seg.f)
#Fit poLCA models
library(poLCA)
set.seed(02807)
seg.LCA3<-poLCA(seg.f, data=seg.df.cut, nclass = 3)
seg.LCA4<-poLCA(seg.f, data=seg.df.cut, nclass=4)
seg.LCA3$bic
seg.LCA4$bic
seg.summ(seg.df, seg.LCA3$predclass)
table(seg.LCA3$predclass)
clusplot(seg.df, seg.LCA3$predclass, color=TRUE, shade=TRUE, labels=4, lines = 0, main="LCA plot (k=3")
seg.summ(seg.df, seg.LCA4$predclass)
table(seg.LCA4$predclass)
clusplot(seg.df, seg.LCA4$predclass, color=TRUE, shade=TRUE, labels=4, lines=0, main="LCA plot (k=4")

#11.3.8
#Comparing Cluster Solutions
table(seg.LCA3$predclass, seg.LCA4$predclass)
library(mclust)
mapClass(seg.LCA3$predclass, seg.LCA4$predclass)

#11.4.1
#Navie Bayes Classification
#Split the data into training and testing
set.seed(04625)
train.prop<-0.65
train.cases<-sample(nrow(seg.raw),nrow(seg.raw)*train.prop)
seg.df.train<-seg.raw[train.cases,]
seg.df.test<-seg.raw[-train.cases,]
library(e1071)
(seg.nb<-naiveBayes(Segment~., data=seg.df.train))
(seg.nb.class<-predict(seg.nb, seg.df.test))
prop.table(table(seg.nb.class))
clusplot(seg.df.test[,-7], seg.nb.class, color=TRUE, shade=TRUE,labels=4, lines=0,
         main="Naive Bayes Classification, holdout data")
mean(seg.df.test$Segment==seg.nb.class)
library(mclust)
adjustedRandIndex(seg.nb.class, seg.df.test$Segment)
#Compare performance for each category
table(seg.nb.class, seg.df.test$Segment)
#Summary data for proposed segments in the test data
seg.sum(seg.df.test, seg.nb.class)
seg.sum(seg.df.test, seg.df.test$Segment)
predict(seg.nb, seg.df.test, type="raw")

#11.4.2 Random Forest Classification
library(randomForest)
set.seed(98040)
(seg.rf<-randomForest(Segment~., data=seg.df.train, ntree=3000))
library(cluster)
seg.rf.class.all<-predict(seg.rf, seg.df.test, predict.all = TRUE)
apply(seg.rf.class.all$individual[1:5,],1,table)/3000

#11.4.3 Random Forest Variable Importance
set.seed(98040)
(seg.rf<-randomForest(Segment~., data=seg.df.train, ntree=3000, importance=TRUE))
importance(seg.rf)
varImpPlot(seg.rf, main="variable Importatnce by Segment")
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(seg.rf)[,1:4]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace = "none", key=FALSE,
          margins = c(10,10),
          main="Variable Importance by segment")

#11.5 Prediction: Identifying Potential Customers
set.seed(92118)
train.prop<-0.65
train.cases<-sample(nrow(seg.df), nrow(seg.df)*train.prop)
sub.df.train<-seg.df[train.cases,]
sub.df.test<-seg.df[-train.cases,]
clusplot(sub.df.train[,-6], sub.df.train$subscribe, color=TRUE, shade=TRUE,
         labels=4, lines=0, main="Subscriber clusters, training data")
#Fit an initial RF model to predict subscribe
library(randomForest)
set.seed(11954)
(sub.rf<-randomForest(subscribe~., data=sub.df.train, ntree=3000))
#Fit another RF model ot predict subscribe
set.seed(11954)
(sub.rf<-randomForest(subscribe~., data=sub.df.train, ntree=3000, sampsize=c(25,25)))
#Apply the RF model to the holdout data and examine the confusion matrix
sub.rf.sub<-predict(sub.rf, sub.df.test)
table(sub.rf.sub, sub.df.test$subscribe)
#Find if the performance is modestly better than chance
adjustedRandIndex(sub.rf.sub, sub.df.test$subscribe)
library(psych)
cohen.kappa(cbind(sub.rf.sub, sub.df.test$subscribe))
