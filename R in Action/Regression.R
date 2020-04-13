#Simple Linear Regression
fit<-lm(weight~height, data=women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)
plot(women$height, women$weight,
     xlab="Height", ylab = "Weight")
abline(fit)

#Polynomial Regression
fit2<-lm(weight~height+I(height^2),data=women)
summary(fit2)
plot(women$height, women$weight,
     xlab="Height",
     ylab="Height")
lines(women$height, fitted(fit2))

#An nth-degree polynomial procuces a curve with n-1 bends
fit3<-lm(weight~height+I(height^2)+I(height^3), data=women)
#Scatterplot
library(car)
scatterplot(weight~height, data=women, spread=FALSE, smoother.args=list(lty=2),pch=19,
            main="WOmen Age 30-39",
            xlab="Height",
            ylab="Weight")

#Multiple Linear Regression
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
summary(fit)
#Examining Bivariate Relationships
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
cor(states)
library(car)
scatterplotMatrix(states, spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")

#Multiple Linear regression with interactions
fit<-lm(mpg~hp+wt+hp:wt, data=mtcars)
summary(fit)

#Regression diagnostics
#confint()
states<-as.data.frame(state.x77[,c('Murder','Population','Illiteracy','Income','Frost')])
fit<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
confint(fit)
#plot
fit<-lm(weight~height, data=women)
par(mfrow=c(2,2))
plot(fit)
fit2<-lm(weight~height+I(height^2), data=women)
par(mfrow=c(2,2))
plot(fit2)
newfit<-lm(weight~height+I(height^2), data=women[-c(13,15),])
plot(newfit)
states<-as.data.frame(state.x77[,c('Murder','Population','Illiteracy','Income','Frost')])
fit<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
par(mfrow=c(2,2))
plot(fit)

#Normality
library(car)
states<-as.data.frame(state.x77[,c('Murder','Population','Illiteracy','Income','Frost')])
fit<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
qqPlot(fit, labels=row.names(states), id.method="identity", simulate=TRUE, main="Q-Q Plot")
states['Nevada',]
fitted(fit)['Nevada']
residuals(fit)['Nevada']
rstudent(fit)['Nevada']
#Function for plotting studentized residuals
residualPlot(fit)
residplot<-function(fit, nbreaks=10){
  z<-rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue",lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red",lwd=2, lty=2)
  legend("topright",
         legend=c("Normal Curve", "Kernel Density Curve"),
         lty=1:2,
         col=c("blue","red"), cex=.7)
}
residplot(fit)
#Independence of Errors
durbinWatsonTest(fit)
#Linearity
library(car)
crPlots(fit)
#Accessing Homoscedasticity
library(car)
ncvTest(fit)
spreadLevelPlot(fit)

#Global validation of linear model assumption
library(gvlma)
gvmodel<-gvlma(fit)
summary(gvmodel)

#Evaluating multicollinearity
library(car)
vif(fit)
sqrt(vif(fit))>2

#Outliers
library(car)
outlierTest(fit)

#High-leverage points
hat.plot<-function(fit){
  p<-length(coefficients(fit))
  n<-length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

#Influential observations
#Cook's D Values
cutoff<-4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col='red')
#Added Variable Plots
library(car)
avPlot(fit, ask=FALSE, id.method='identify')
#Influential plot
library(car)
influencePlot(fit, id.method="identify",main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

#Box-Cox transformation to normality
library(car)
boxTidwell(Murder~Population+Illiteracy, data=states)

#Selecting the "Best" regression model
#Comparing nested models with the anova() function
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit1<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
fit2<-lm(Murder~Population+Illiteracy, data=states)
anova(fit2,fit1)
#Comparing models with the AIC
fit1<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
fit2<-lm(Murder~Population+Illiteracy, data=states)
AIC(fit1,fit2)

#Variable selection
#Backward stepwise selection
library(MASS)
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
stepAIC(fit, direction = "backward")
#All Subset Regression
library(leaps)
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
leaps<-regsubsets(Murder~Population+Illiteracy+Income+Frost, data=states, nbest=4)
plot(leaps, scale="adjr2")
library(car)
subsets(leaps, statistic = "cp",
      main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2, col="red")

#k-fold cross-validated R-square
shrinkage<-function(fit, k=10){
  require(bootstrap)
  theta.fit<-function(x,y){lsfit(x,y)}
  theta.predict<-function(fit,x){cbind(1,x)%*%fit$coef}
  x<-fit$model[,2:ncol(fit$model)]
  y<-fit$model[,1]
  
  results<-crossval(x,y,theta.fit, theta.predict, ngroup=k)
  r2<-cor(y, fit$fitted.values)^2
  r2cv<-cor(y, results$cv.fit)^2
  cat("Origin R-square=", r2,"\n")
  cat(k, "Fold Cross-Validated R-square=", r2cv, "\n")
  cat("Change=", r2-r2cv,"\n")
}
states<-as.data.frame(state.x77[,c("Murder","Population",
                                   "Illiteracy","Income","Frost")])
fit<-lm(Murder~Population+Income+Illiteracy+Frost, data=states)
shrinkage(fit)
