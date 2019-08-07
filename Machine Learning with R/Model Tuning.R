#Find the tuning parameters for a particular model
modelLookup("C5.0")

#Createing a simple tuned model
library(caret)
set.seed(300)
credit<-read.csv("credit.csv")
m<-train(as.factor(default)~., data=credit, method="C5.0")
m
p<-predict(m, credit)
table(p, credit$default)
head(predict(m, credit))
head(predict(m, credit, type="prob"))

#Customizing the tuning process
#Create a control object that uses 10-fold cross-validation
ctrl<-trainControl(method="cv", number=10, selectionFunction="oneSE")

#Define the grid of parameters to optimize
grid<-expand.grid(model="tree",
                  trials=c(1,5,10,15,20,25,30,35),
                  winnow="FALSE")
grid

#Tune the model
set.seed(300)
m<-train(as.factor(default)~., data=credit, method="C5.0", metric="Kappa",trControl=ctrl, tunegrid=grid)
m
p2<-predict(m, credit)
table(p2, credit$default)

#Set and Tune parameters
grid=expand.grid(C=c(0.1,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5))
trctrl=trainControl(method="cv", number=10)

#Train Cross-validation, linear SVM
start.time=Sys.time()
cv.svm.l=train(quality~., data=train, method="svmLinear", trControl=trctrl, preProcess=c("center","scale"), tuneGrid=grid, tuneLength=10 )
end.time=Sys.time()
time.taken=end.time-start.time
time.taken
cv.svm.l
train_cv.svm.l=predict(cv.svm.l, newdata=train)
confusionMatrix(train_cv.svm.l, train$quality)
plot(train_cv.svm.l)
plot(cv.svm.l)
