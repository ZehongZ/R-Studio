#Exhibit 3.1 Least Square Regression Estimates for Linear Time Trend
library(TSA)
data("rwalk")
model1=lm(rwalk~time(rwalk))
summary(model1)

#Exhibit 3.2 
#Random Walk with Linear Time Trend
plot(rwalk, type='o',ylab='y')
abline(model1)

#Exhibit 3.3
#Regression Results for the Seasonal Means Model
data("tempdub")
month=season(tempdub)#Period added to improve table display
model2=lm(tempdub~month-1)#-1 removes the intercept term
summary(model2)
model3=lm(tempdub~month)
summary(model3)

#Exhibit 3.5
#Cosine Trend Model for Temperature Series
har=harmonic(tempdub,1)
model4=lm(tempdub~har)
summary(model4)

#Exhibit 3.6
#Cosine Trend for the Temperature Series
plot(ts(fitted(model4), freq=12, start = c(1964,1)), ylab="Temperature", type="l", ylim=range(c(fitted(model4), tempdub)))
points(tempdub)

#Exhibit 3.7
library(TSA)
data("rwalk")
model1=lm(rwalk~time(rwalk))
summary(model1)

#Exhibit 3.8
#Residuals versus Time for Temperature Seasonal Means
plot(y=rstudent(model3),x=as.vector(time(tempdub)),xlab='Time',ylab='Standardized Residuals',type='o')

#Exhibit 3.9
#Residual versus Time with Seasonal Plotting Symbols
plot(y=rstudent(model3),x=as.vector(time(tempdub)),xlab='Time',ylab='Standardized Residuals',type='l')
points(y=rstudent(model3),x=as.vector(time(tempdub)),pch=as.vector(season(tempdub)))

#Exhibit 3.10
#Standardized residuals versus Fitted Values for the Temperature Seasonal Means Model
plot(y=rstudent(model3),x=as.vector(fitted(model3)),xlab='Fitted Trend Values',ylab='Standardized Residuals',type='n')
points(y=rstudent(model3),x=as.vector(fitted(model3)),pch=as.vector(season(tempdub)))

#Exhibit 3.11
#Histogram of Standardized Residuals from Seasonal Means Model
hist(rstudent(model3),xlab='Standardized Residuals')

#Exhibit 3.12
#Q-Q Plot: Standardized Residuals of Seasonal Means Model
library(graphics)
qqnorm(rstudent(model3))#Support the assumption of a normally distributed stochastic component

#Exhibit 3.13 
#Sample Autocorrelation of Residuals of Seasonal Means Model
acf(rstudent(model3))#The series is a white noise

#Exhibit 3.14
#Residuals from straight Line Fit of the Random Walk
plot(y=rstudent(model1),x=as.vector(time(rwalk)),ylab='Standardized Residuals',xlab='Time',type='o')

#Exhibit 3.15
#Residuals versus Fitted Values from Straight Line Fit
plot(y=rstudent(model1),x=fitted(model1),ylab='Standardized Residuals',xlab='Fitted Trend Line Vlaues', type='p')

#Exhibit 3.16
#Sample Autocorrelation of Residuals from Straight Line Model
acf(rstudent(model1))
#Not a white noise, meanining series needs to be improved.

#Exhitbit 3.17
#Quantile-Quantile Plot of Los Angeles Annual Rainfall Series
data("larain")
qqnorm(larain)
qqline(larain)
