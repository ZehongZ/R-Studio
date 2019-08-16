#RNN model
#Install package
library(rnn)
#Create a set of random numbers in X1 and X2
x1=sample(0:127, 7000, replace=TRUE)
x2=sample(0:127, 7000, replace=TRUE)
#Create training response numbers
y=x1+x2
#Convert to binary
x1=int2bin(x1)
x2=int2bin(x2)
y=int2bin(y)
#Create 3d array: dim1:samples; dim2: time; dim 3:variables
x=array(c(x1,x2),dim=c(dim(x1),2))
x
#Train the model
model<-trainr(Y=y[,dim(y)[2]:1],
              X=x[,dim(x)[2]:1,],
              learningrate = 0.1,
              batch_size = 100,
              numepochs = 100)
#Visualize model
plot(colMeans(model$error),type='l',xlab='epoch',ylab='errors')
#Create test inputs
A1=int2bin(sample(0:127, 7000, replace = TRUE))
A2=int2bin(sample(0:127, 7000, replace = TRUE))
#Create 3d array:dim 1:samples; dim 2: time; dim 3: variables
A=array(c(A1,A2), dim=c(dim(A1),2))
#Run prediction for new A
B=predictr(model,
           A[,dim(A)[2]:1,])
B=B[,dim(B)[2]:1]
#Convert back to integers
A1=bin2int(A1)
A2=bin2int(A2)
B=bin2int(B)
#Plot the differences as hitogram
hist(B-(A1+A2))

#Humidity forecasting with RNNs
library(rattle.data)
library(rnn)
#Import data
data("weatherAUS")
#Extract only 1 and 14 column and first 3040 rows
data=weatherAUS[1:3040, c(1,14)]
summary(data)
#Clean the datasets
data_cleaned<-na.omit(data)
data_used=data_cleaned[1:3000,]
x=data_cleaned[,1]
y=data_cleaned[,2]
head(x)
head(y)
#Transform it into matrix
X=matrix(x, nrow=30)
Y=matrix(y, nrow=30)
#Standardize in the interval 0-1
Yscaled=(Y-min(Y))/(max(Y)-min(Y))
Y=t(Yscaled)
train=1:70
test=71:100
#Train the model
model<-trainr(Y=Y[train,],
              X=Y[train,],
              learningrate = 0.05,
              hidden_dim = 16,
              numepochs = 1000)
plot(colMeans(model$error), type = "l", xlab="epoch",ylab = "errors")
Yp<-predictr(model, Y[test,])
#Visualize the model
plot(as.vector(t(Y[test,])), col="red",type="l",
     main="Actual vs Predicted Humidity: testing set",
     ylab="Y,Yp")
lines(as.vector(t(Yp)), type="l", col="black")
legend("bottomright", c("Predicted","Actual"),
       col=c("red","black"),
       lty=c(1,1),lwd=c(1,1))


