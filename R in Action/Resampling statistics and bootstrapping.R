#Independent two-sample and k-sample tests
#t-test vs. one-way permutation test for the hypothetical data
library(coin)
score<-c(40,57,45,55,58,57,64,55,62,65)
treatment<-factor(c(rep("A",5), rep("B",5)))
mydata<-data.frame(treatment, score)
t.test(score~treatment, data=mydata, var.equal=TRUE)
oneway_test(score~treatment, data=mydata, distribution="exact")
#Wilcoxon-Mann-Whitney U test
library(MASS)
UScrime<-transform(UScrime, So=factor(So))
wilcox_test(Prob~So, data=UScrime, distribution="exact")

#Independece in contingency tables
#Permutation version of the chi-square test
library(coin)
library(grid)
library(vcd)
Arthritis<-transform(Arthritis, Improved=as.factor(as.numeric(Improved)))
set.seed(1234)
chisq_test(Treatment~Improved, data=Arthritis,
           distribution=approximate(B=9999))

#Independece between numeric variables
states<-as.data.frame(state.x77)
set.seed(1234)
spearman_test(Illiteracy~Murder, data=states,
              distribution=approximate(B=9999))

#Dependent two-sample and k-sample tests
library(coin)
library(MASS)
wilcoxsign_test(U1~U2, data=UScrime, distribution="exact")

#Permutation tests for simple linear regression
library(lmPerm)
set.seed(1234)
fit<-lmp(weight~height, data = women, perm="Prob")
summary(fit)

#Permutation tests for polynomial regression
library(lmPerm)
set.seed(1234)
fit<-lmp(weight~height+I(height^2), data=women, perm="Prob")
summary(fit)

#Permutation tests for multiple regression
library(lmPerm)
set.seed(1234)
states<-as.data.frame(state.x77)
fit<-lmp(Murder~Population+Illiteracy+Income+Frost, 
         data=states, perm="Prob")
summary(fit)

#Permutation test for one-way ANOVA
library(lmPerm)
set.seed(1234)
fit<-aovp(weight~gesttime+dose, data=litter, perm="Prob")

#Permutation test for two-way ANOVA
library(lmPerm)
set.seed(1234)
fit<-aovp(len~supp*dose, data=ToothGrowth, perm="Prob")
anova(fit)

#Bootstrapping a single statistic
#R-square function
rsq<-function(formula, data, indices){
  d<-data[indices,]
  fit<-lm(formula, data=d)
  return(summary(fit)$r.square)
}
#Bootstrap
library(boot)
set.seed(1234)
results<-boot(data=mtcars, statistic=rsq,
              R=1000, formula=mpg~wt+disp)
print(results)
plot(results, index=1)
#A 95% confidence interval for the R-squared values
boot.ci(results, type=c("perc","bca"),index=1)
boot.ci(results, type=c("bca"), index=1)
