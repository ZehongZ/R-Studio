#Generalized linear models extended the liner-model framework to include dependent variables that are decidedly non-normal
#Two popular models: logistic regression and Posson regression

#Logistic Regression
#Useful for binary outcome.
library(AER)
data("Affairs")
summary(Affairs)
table(Affairs$affairs)
Affairs$yaffairs[Affairs$affairs>0]<-1
Affairs$yaffairs[Affairs$affairs==0]<-0
Affairs$yaffairs<-factor(Affairs$yaffairs, levels = c(0,1), labels = c("No","Yes"))
table(Affairs$yaffairs)
#Build logistic regression
fit.full<-glm(yaffairs~gender+age+yearsmarried+children+religiousness+education+occupation+rating, data=Affairs, family=binomial())
summary(fit.full)

fit.reduced<-glm(yaffairs~age+yearsmarried+religiousness+rating, data=Affairs, family=binomial())
summary(fit.reduced)

#Using ANOVA to compare fit.reduce and fit.full
#Using chi-sq for generalized linear models
anova(fit.reduced, fit.full, test="Chisq")
#Two models are not significant difference 

#Interpreting the model parameters
coef(fit.reduced)

exp(coef(fit.reduced))

#Assessing the impact of predictors on the probability of an outcome
testdata<-data.frame(rating=c(1,2,3,4,5), age=mean(Affairs$age), yearsmarried=mean(Affairs$yearsmarried), religiousness=mean(Affairs$religiousness))
testdata

#Using the test dataset and prediction equation to obtain probabilities
testdata$prob<-predict(fit.reduced, newdata=testdata, type="response")
testdata

#Look at the impact of age
testdata<-data.frame(rating=mean(Affairs$rating), age=seq(17,57,10), yearsmarried=mean(Affairs$yearsmarried), religiousness=mean(Affairs$religiousness))
testdata
testdata$prob<-predict(fit.reduced, newdata = testdata, type="response")
testdata

#Overdispersion
#Occurs when the observed variance of the response variable is larger than what would be expected from a binomial distribution.
#Overdispersion can lead to distorted test standard errors and inaccurate test of significance
deviance(fit.reduced)/df.residual(fit.reduced)
#Result closes to 1 suggesting no overdispersion

#Test for overdispersion
pchisq(summary(fit.od)$dispersion*fit$df.residual, fit$df.residual, lower=F)
fit<-glm(ynaffair~age+yearsmarried+religiousness+rating, family=binomial(),data=Affairs)
#The resulting p-value 0.34 is not significant so overdispersion isn't a problem

#Poisson regression
library(robust)
data("breslow.dat")
names(breslow.dat)
summary(breslow.dat[c(6,7,8,10)])
opar<-par(no.readonly = TRUE)
par(mfrow=c(1,2))
attach(breslow.dat)
hist(sumY, breaks = 20, xlab="Seizure Count",main="Distribution of Seizures")
boxplot(sumY~Trt, xlab="Treatment", main="Group Comparisons")
par(opar)
fit<-glm(sumY~Base+Age+Trt, data=breslow.dat,family = poisson())
summary(fit)
#Interpreting the model parameters
coef(fit)
exp(coef(fit))
#Overdispersion
deviance(fit)/df.residual(fit)
#Test for overdispersion in the Poisson case
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY, type="poisson")
fit.od<-glm(sumY~Base+Age+Trt, data=breslow.dat, family = quasipoisson())
summary(fit.od)
fit<-glm(sumY~Base+Age+Trt, data=breslow.dat, offset=log(time), family = poisson)
