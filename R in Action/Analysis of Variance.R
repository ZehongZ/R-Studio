#Install packages
library(car)
library(gplots)
library(HH)
library(rrcov)
library(multicomp)
library(multcomp)

#One-way ANOVA
library(multcomp)
attach(cholesterol)
summary(cholesterol)
table(trt)
#Group means
aggregate(response, by=list(trt), FUN=mean)
#Group standard deviations
aggregate(response, by=list(trt), FUN=sd)
#Tests for group differences(ANOVA)
fit<-aov(response~trt)
summary(fit)
library(gplots)
plotmeans(response~trt, xlab="Treatment",ylab="Response",main="Mean Plot\nwith 95% CI")
detach(cholesterol)

#Turkey HSD pairwise group comparisons
TukeyHSD(fit)
par(las=2)#Rotate the bar
par(mar=c(5,8,4,2))
plot(TukeyHSD(fit))
#Method 2
library(multcomp)
par(mar=c(5,4,6,2))
tuk<-glht(fit, linfct=mcp(trt="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")

#Assessing test assumptions
#Normality
library(car)
qqPlot(lm(response~trt, data=cholesterol), 
       sumulate=TRUE, main="Q-Q Plot", labels=FALSE)
#Equality (homogeneity) of variance
bartlett.test(response~trt, data=cholesterol)
#Outliers
library(car)
outlierTest(fit)

#One-way ANCOVA
data("litter", package = "multcomp")
attach(litter)
table(dose)
aggregate(weight, by=list(dose), FUN=mean)
fit<-aov(weight~gesttime+dose)
summary(fit)

#Multiple comparisons enploying user-supplied contrasts
library(multcomp)
contrast<-rbind("no drug vs. drug"=c(3,-1,-1,-1))
summary(glht(fit, linfct=mcp(dose=contrast)))

#Assessing test assumptions
#Testing for homogeneity of regression slopes
library(multcomp)
fit2<-aov(weight~gesttime*dose, data=litter)
summary(fit2)

#Visualizing the results
library(HH)
ancova(weight~gesttime+dose, data=litter)

#Two-way ANOVA
attach(ToothGrowth)
table(supp,dose)
aggregate(len, by=list(supp, dose), FUN=mean)
aggregate(len, by=list(supp, dose), FUN=sd)
dose<-factor(dose)
fit<-aov(len~supp*dose)
summary(fit)
detach(ToothGrowth)
#Visualize the results
attach(ToothGrowth)
interaction.plot(dose, supp, len, type="b",
                 col=c("red","blue"), pch=c(16,18),
                 main="Interaction between Dose and Supplement Type")
detach(ToothGrowth)
#MInteraction plot
library(gplots)
attach(ToothGrowth)
plotmeans(len~interaction(supp, dose, sep=""),
         connect=list(c(1,3,5), c(2,4,6)),
         col=c("red","darkgreen"),
         main="Interaction Plot with 95% CIs",
         xlab="Treatment and Dose Combination")
detach(ToothGrowth)
#Method 3
library(HH)
attach(ToothGrowth)
interaction2wt(len~supp*dose)
detach(ToothGrowth)

#Repeated measures ANOVA with one between and within-groups factor
CO2$conc<-factor(CO2$conc)
w1b1<-subset(CO2, Treatment=="chilled")
fit<-aov(uptake~conc*Type+Error(Plant/(conc)),w1b1)
summary(fit)
par(las=2)
par(mar=c(10,4,4,2))
with(w1b1, interaction.plot(conc, Type, uptake,
                            type="b", col=c("red","blue"), pch=c(16,18),
                            main="Interaction Plot for Plant Type and Concentration"))
boxplot(uptake~Type*conc, data=w1b1, col=(c("gold","green")),
        main="Chilled Quebec and Mississippi Plants",
        ylab="Carbon dioxide uptake rate (umol/m^2 sec)")

#Multivariate analysis of variance (MANOVA)
#One-way MANOVA
library(MASS)
attach(UScereal)
shelf<-factor(shelf)
y<-cbind(calories, fat, sugars)
aggregate(y, by=list(shelf), FUN=mean)
cov(y)
fit<-manova(y~shelf)
summary(fit)
summary.aov(fit)
#Assess multivariate normality
center<-colMeans(y)
n<-nrow(y)
p<-ncol(y)
cov<-cov(y)
d<-mahalanobis(y, center, cov)
coord<-qqplot(qchisq(ppoints(n), df=p),
              d, main="Q-Q Plot Assessing Multivariate Normality",
              ylab="Mahalanobis D2")
abline(a=0, b=1)
identify(coord$x, coord$y, labels = row.names(UScereal))

#Robust MANOVA
library(rrcov)
Wilks.test(y, shelf, method="mcd")

#ANOVA as regression
library(multcomp)
levels(cholesterol$trt)
fit.aov<-aov(response~trt, data=cholesterol)
summary(fit.aov)
#A regression approach to the ANOVA
fit.lm<-lm(response~trt, data=cholesterol)
summary(fit.lm)
contrasts(cholesterol$trt)
fit.lm<-lm(response~trt, data=cholesterol, cotrasts="contr.helmert")
options(contrasts = c("contr.SAS","contr.helmert"))
