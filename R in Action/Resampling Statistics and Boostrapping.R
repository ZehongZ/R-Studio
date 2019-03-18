#Permutation Tests
##Test to see the difference are significant for two groups
##Parametric approach: assume data are sampled from normal populations with equal variances and apply a two-tailed independent-groups t-test
##Permutation approach compares to an empirical distribution created from permutations of the observed data
##Permutation is an alterantive if data is not normally distributed, has outliers and dataset is too small

#Independent two-sample and k-sample test
library(coin)
score<-c(40,57,45,55,58,57,64,55,62,65)
treatment<-factor(c(rep("A",5), rep("B",5)))
mydata<-data.frame(treatment, score)
t.test(score~treatment, data=mydata, var.equal=TRUE)
#Trational t-test p=0.047;

oneway_test(score~treatment, data=mydata, distribution="exact")
#Treatment t-tets p=0.07143. Trust more on Treatment t-test and suggest more data collection

#Exact Wilcoxon rank-sum test
library(MASS)
UScrime<-transform(UScrime, So=factor(So))
wilcox_test(Prob~So, data=UScrime, distribution="exact")

#K-sample permutation (vs. one-way ANOVA)
library(multcomp)
set.seed(1234)
oneway_test(response~trt, data=cholesterol, distribution=approximate(B=9999))

#Indenpendce in contingency tables
##Assess the independence of two categorical variables
library(coin)
library(vcd)
Arthritis<-transform(Arthritis, Improved=as.factor(as.numeric(Improved)))
set.seed(1234)
chisq_test(Treatment~Improved, data=Arthritis, distribution=approximate(B=9999))

#Independence between numeric variables
states<-as.data.frame(state.x77)
set.seed(1234)
spearman_test(Illiteracy~Murder, data=states, distribution=approximate(B=9999))
##Reject hypothesis of independence

#Dependent two-sample and k-sample test
library(coin)
library(MASS)
wilcoxsign_test(U1~U2, data=UScrime, distribution="exact")

#Simple and polynomial regression
library(lmPerm)
set.seed(1234)
fit<-lmp(weight~height, data=women, perm="Prob")
summary(fit)

#Permutation tests for polynomial
library(lmPerm)
set.seed(1234)
fit<-lm(weight~height+I(height^2), data=women, perm="Prob")
summary(fit)

#Permutation tests for multiple regression
library(lmPerm)
set.seed(1234)
states<-as.data.frame(state.x77)
fit<-lmp(Murder~Population+Illiteracy+Income+Frost, data=states, perm="Prob")
summary(fit)

#Permutation test for one-way ANOVA
library(lmPerm)
library(multcomp)
set.seed(1234)
fit<-aovp(response~trt, data=cholesterol, perm="Prob")
anova(fit)

#Permutation test for one-way ANCOVA
library(lmPerm)
set.seed(1234)
fit<-aovp(weight~gesttime+dose, data=litter, perm="Prob")
anova(fit)

#Two-way ANOVA
library(lmPerm)
set.seed(1234)
fit<-aovp(len~supp*dose, data=ToothGrowth, perm="Prob")
anova(fit)

#Bootstrapping a single statistic
##Bootstrapping generates an empirical distribution of a test statistic or set of test statistics by repeated random sampling with replacement from the original sample. It allows you to generate confidence intervals and test statistical hypotheses without having to assume a specific underlying theoretical distribution.
library(boot)
set.seed(1234)
rsq <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}
results<-boot(data=mtcars, statistic = rsq, R=1000, formula=mpg~wt+disp)
print(results)
plot(results)

#Obtain 95% confidence interval for R-squared values
boot.ci(results, type=c("perc","bca"))

#Bootstrapping serveral statistics
bs <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(coef(fit))
}
library(boot)
set.seed(1234)
results<-boot(data=mtcars, statistic=bs, R=1000, formula=mpg~wt+disp)
print(results)
plot(results, index=2)
boot.ci(results, type="bca",index=2)
boot.ci(results, type="bca",index=3)
