#Simple Perceptron Implementation function
data("iris")
#First 20 rows of datasets
head(iris, n=20)
str(iris)
#Subset the dataset
iris_sub=iris[1:100, c(1,3,5)]
#Name the variables
names(iris_sub)=c("sepal","petal","species")
#First 5 rows of the subset
head(iris_sub)
#Install library
library(ggplot2)
ggplot(iris_sub, aes(x=sepal, y=petal))+
  geom_point(aes(colour=species, shape=species),size=3)+
  xlab("Sepal length")+
  ylab("Petal length")+
  ggtitle("Species vs Sepal and Petal Lengths")
#Euclidean normalization function
euclidean.norm=function(x) {sqrt(sum(x*x))}
#Distance from plan function
distance.from.plane=function(z,w,b){
  sum(z*w)+b
}
#Linear classification function
classify.linear=function(x,w,b){
  distance=apply(x,1,distance.from.plane,w,b)
  return(ifelse(distance<0,-1,+1))
}
#Perceptron
perceptron=function(x,y,learning.rate=1){
  w=vector(length = ncol(x))#Initialize weights
  b=0 #Initialize bias
  k=0 #count updates
  R=max(apply(x,1,euclidean.norm))
  mark.complete=TRUE
  while (mark.complete){
    mark.complete=FALSE
    yc=classify.linear(x,w,b)
    for (i in 1:nrow(x)){
      if (y[i]!=yc[i]){
        w=w+learning.rate*y[i]*x[i,]
        b=b+learning.rate*y[i]*R^2
        k=k+1
        mark.complete=TRUE
      }
    }
  }
  s=euclidean.norm(w)
  return(list(w=w/s,b=b/s,update=k))
}
x=cbind(iris_sub$sepal, iris_sub$petal)
y=ifelse(iris_sub$species=="setosa",+1,-1)
p=perceptron(x,y)
plot(x, cex=0.2)
points(subset(x,y==1), col="black", pch="+",cex=2)
points(subset(x,y==-1),col="red",pch="-",cex=2)
intercept=-p$b/p$w[[2]]
slope=-p$w[[1]]/p$w[[2]]
abline(intercept, slope, col="green")

#Simple RSNNS Implementation
#Import dataset
data("iris")
#Install library
library(RSNNS)
iris=iris[sample(1:nrow(iris),length(1:nrow(iris))),1:ncol(iris)]
irisValues=iris[,1:4]
irisTargets=decodeClassLabels(iris[,5])
iris=splitForTrainingAndTest(irisValues, irisTargets, ratio = 0.15)
iris=normTrainingAndTestSet(iris)
model=mlp(iris$inputsTrain,
          iris$targetsTrain,
          size=5,
          learnFuncParams = c(0,1),
          maxit=50,
          inputsTest = iris$inputsTest,
          targetsTest = iris$targetsTest)
summary(model)
weightMatrix(model)
par(mfrow=c(2,2))
plotIterativeError(model)
predictions=predict(model, iris$inputsTest)
plotRegressionError(predictions[,2],iris$targetsTest[,2])
confusionMatrix(iris$targetsTrain, fitted.values(model))
confusionMatrix(iris$targetsTest,predictions)
par(mfrow=c(1,2))
plotROC(fitted.values(model)[,2], iris$targetsTrain[,2])
plotROC(predictions[,2], iris$targetsTest[,2])
confusionMatrix(iris$targetsTrain,
                encodeClassLabels(fitted.values(model),
                                  method="402040",
                                  l=0.4,
                                  h=0.6))
