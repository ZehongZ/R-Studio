#Import the dataset
sat.df <- read.csv("http://goo.gl/HKnl74")
#Overview of the dataset
str(sat.df)
summary(sat.df)

#7.2.1
#Preliminary Data Inspection
#Initial check for distributions and relationships
gpairs::gpairs(sat.df)
#Transform the distant to make to closer to normal distributions
sat.df$logdist<-log(sat.df$distance)
#Investing highly correlated variables
library(corrplot)
corrplot.mixed(cor(sat.df[,c(2, 4:9)]), upper = "ellipse")

#7.2.2
#Bivariate association
plot(overall~rides, data=sat.df, xlab="Satisfaction with Rides", ylab = "Overall Satisfaction")

#7.3.3
#Linear Model with a Single Predictor
lm(overall~rides, data=sat.df)

#7.2.4
#lm Objects
m1<-lm(overall~rides, data=sat.df)
plot(overall~rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")
abline(m1, col="blue")
m1$coefficients
summary(m1)
confint(m1)
cor(sat.df$overall, sat.df$rides)^2

#7.2.5
#Checking Model Fit
#Check if there is linear relationships
x<-rnorm(500)
y<-x^2+rnorm(500)
toy.model<-lm(y~x)
plot(y~x)
abline(toy.model)
summary(toy.model)
plot(y~x)
#Check the distributions of errors
plot(toy.model$fitted.values, toy.model$residuals)
#Diagnostic plot for objects
par(mfrow=c(2,2))
plot(m1)
#Check the outliers
sat.df[c(57,129,295),]

#7.3
#Fitting Linear Models with Multiple Predictors
m2<-lm(overall~rides+games+wait+clean, data=sat.df)
summary(m2)
#Inspect coefficients
library(coefplot)
coefplot(m2, intercept=FALSE, outerCI=1.96, lwdOuer=1.5,
         ylab="Rating of Feature",
         xlab="Association with Overall Satisfaction")

#7.3.1
#Comparing Models
#Evalutate by R-squared values
summary(m1)$r.squared
summary(m2)$r.squared
#Evaluate by Adjusted R-squared
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared
#Compare the predictions of the models visually
plot(sat.df$overall, fitted(m1), col="red", xlim=c(0,100), ylim=c(0,100),
     xlab="Actuall Overall Satisfaction", ylab="Fitted Overall Satisfaction")
points(sat.df$overall, fitted(m2), col="blue")
legend("topleft", legend = c("model 1","model 2"),col=c("red","blue"),pch=1)
#Evaluate by using ANOVA
anova(m1, m2)

#7.3.2
#Using a Model to Make Predictions
coef(m2)["(Intercept)"]+coef(m2)["rides"]*100+coef(m2)["games"]*100+coef(m2)["wait"]*100+coef(m2)["clean"]*100
coef(m2)%*%c(1,100,100,100,100)
predict(m2, sat.df[1:10,])
#The best estimate is 90.586 using model m2
fitted(m2)[1:10]

#7.3.3
#Standardzing the Predictors
(sat.df$rides-mean(sat.df$rides))/sd(sat.df$rides)
scale(sat.df$rides)
#Remove non-relative variables
sat.std<-sat.df[,-3]
dim(sat.std)
sat.std[,3:8]<-scale(sat.std[,3:8])
head(sat.std)
summary(sat.std)

#7.4
#Using Factors as Predictors
m3<-lm(overall~rides+games+wait+clean+weekend+logdist+num.child, data=sat.std)
summary(m3)
#Converting num.child to a factor
sat.std$num.child.factor<-factor(sat.std$num.child)
m4<-lm(overall~rides+games+wait+clean+weekend+logdist+num.child.factor, data=sat.std)
summary(m4)
#Create a new variable has.child and drop weekend due to non-significant
sat.std$has.child<-factor(sat.std$num.child>0)
m5<-lm(overall~rides+games+wait+clean+logdist+has.child, data=sat.std)
summary(m5)

#7.5
#Interaction Terms
m6<-lm(overall~rides+games+wait+clean+weekend+logdist+has.child+rides:has.child+games:has.child+wait:has.child+clean:has.child+rides:weekend+games:weekend+wait:weekend+clean:weekend, data=sat.std)
summary(m6)
#Drop insignificant variables and build m7
m7<-lm(overall~rides+games+wait+clean+logdist+has.child+wait:has.child, data=sat.std)
summary(m7)
library(coefplot)
coefplot(m7, intercept=FALSE, outerCI=1.96, lwdOuter=1.5, ylab="Rating of Feature", xlab="Association with Overall Satisfaction")

