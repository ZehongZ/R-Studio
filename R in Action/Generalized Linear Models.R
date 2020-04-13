#Logistic regression
data(Affairs, package="AER")
summary(Affairs)
table(Affairs$affairs)
Affairs$ynaffair[Affairs$affairs>0]<-1
Affairs$ynaffair[Affairs$affairs==0]<-0
Affairs$ynaffair<-factor(Affairs$ynaffair, levels = c(0,1),labels=c("No","Yes"))
table(Affairs$ynaffair)
#Logitstic regression
fit.full<-glm(ynaffair~gender+age+yearsmarried+children+religiousness+education+occupation+rating, data=Affairs, family=binomial())
summary(fit.full)
#Remove insignificant variables
fit.reduced<-glm(ynaffair~age+yearsmarried+religiousness+rating, data=Affairs, family = binomial())
summary(fit.reduced)
#Chi-square
anova(fit.reduced, fit.full, test="Chisq")
#Interpreting the model parameters
coef(fit.reduced)
exp(coef(fit.reduced))

#Assessing the impact of predictors on the probability of an outcome
testdata<-data.frame(rating=c(1,2,3,4,5), age=mean(Affairs$age),
                     yearsmarried=mean(Affairs$yearsmarried),
                     religiousness=mean(Affairs$religiousness))
testdata
testdata$prob<-predict(fit.reduced, newdata=testdata, type="response")
testdata<-data.frame(rating=mean(Affairs$rating),
                     age=seq(17,57,10),
                     yearsmarried=mean(Affairs$yearsmarried),
                     religiousness=mean(Affairs$religiousness))
testdata
testdata$prob<-predict(fit.reduced, newdata=testdata, type="response")
testdata

#Overdispersion
deviance(fit.reduced)/df.residual(fit.reduced)
fit<-glm(ynaffair~age+yearsmarried+religiousness+rating, family=binomial(),data=Affairs)
fit.od<-glm(ynaffair~age+yearsmarried+religiousness+rating, family=quasibinomial(),data=Affairs)
pchisq(summary(fit.od)$dispersion*fit$df.residual, fit$df.residual, lower=F)

#Poisson regression
data(breslow.dat, package="robust")
summary(breslow.dat[c(6,7,8,10)])
#Graph
opar<-par(no.readonly = TRUE)
par(mfrow=c(1,2))
attach(breslow.dat)
hist(sumY, breaks=20, xlab="Seizure Count",
     main="Distribution of Seizures")
boxplot(sumY~Trt, xlab="Treatment", main="Group Comparions")
par(opar)
#Poisson regression
fit<-glm(sumY~Base+Age+Trt, data=breslow.dat, family = poisson())
summary(fit)

#Interpreting the model parameters
coef(fit)
exp(coef(fit))

#Ovredispersin
deviance(fit)/df.residual(fit)
#Test Overdispersion
library(qcc)
qcc.overdispersion.test(breslow.dat, type = "poisson")
fit.od<-glm(sumY~Base+Age+Trt, data=breslow.dat, family = quasipoisson())
summary(fit.od)
