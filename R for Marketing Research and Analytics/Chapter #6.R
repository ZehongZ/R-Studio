#6.1
#Data for Comparing Groups
#Import dataset
seg.df <- read.csv("http://goo.gl/qw303p")
summary(seg.df)

#6.2
#Testing Group Frequencies: chisq.test()
#Null hypothesis: No difference between the cess with 95% confidence
tmp.tab<-table(rep(c(1:4), times=c(25,25,25,20)))
tmp.tab
chisq.test(tmp.tab)
#Cannot reject null hypothesis, the data shows no evidence that the groups in the population are of unequal size, under the assumption of random sampling

tmp.tab<-table(rep(c(1:4), times=c(25,25,25,10)))
tmp.tab
chisq.test(tmp.tab)
#Reject null hypothesis, there is a difference between groups
chisq.test(table(seg.df$Segment))
#Reject null hypothesis, there are differences in segment size
#There isn't an identical number of customers in each segment

#Is subscription status independent from home ownership?
table(seg.df$subscribe, seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))
#If these two are related, then the number should be close

#6.3
#Testing Observed Proportions: binom.test()
binom.test(12,20,p=0.5)

#6.3.2
#More about binom.test() and Binomial Distributions
binom.test(120,200, p=0.5)
sum(dbinom(8:12, 20, 0.5))
library(binom)
binom.confint(12, 20, method="ac")
binom.confint(0, 20, method="ac")

#6.4
#Testing Group Means: t.test()
hist(seg.df$income)
with(seg.df, hist(income[ownHome=="ownYes"]))
with(seg.df, hist(income[ownHome=="ownNo"]))
t.test(income~ownHome, data=seg.df)
#Null hypothesis of no difference in income by ownership is rejected

t.test(income~ownHome, data=subset(seg.df, Segment=="Travelers"))

#6.5
#Testing Multiple Group Means: ANOVA
seg.aov.own<-aov(income~ownHome, data=seg.df)
anova(seg.aov.own)
seg.aov.seg<-aov(income~Segment, data=seg.df)
anova(seg.aov.seg)
anova(aov(income~Segment+ownHome, data=seg.df))
anova(aov(income~Segment*ownHome, data=seg.df))

#6.5.1
#Model Comparison in ANOVA
anova(aov(income~Segment, data=seg.df),
      aov(income~Segment+ownHome, data=seg.df))

#6.5.2
#Visualizing Group Confidence Intervals
library(multcomp)
seg.aov<-aov(income~Segment, data=seg.df)
glht(seg.aov)

seg.aov<-aov(income~-1+Segment, data=seg.df)
glht(seg.aov)

par(mar=c(6,10,2,2))
plot(glht(seg.aov),
     xlab="Income", main="Average Income by Segment (95% CI)")

seg.aov.step<-step(aov(income~., data=seg.df))
anova(seg.aov.step)

#6.6.2
set.seed(96761)
library(BayesFactor)
seg.bf1<-lmBF(income~Segment, data=seg.df)
seg.bf2<-lmBF(income~Segment+ownHome, data=seg.df)
seg.bf1/seg.bf2
#The model 1 is the preferable model by a factor of 6.2
seg.bf.chain<-posterior(seg.bf1, 1, iterations = 10000)
plot(seg.bf.chain[,1:6])

#6.6.3
#Inspecting the Posterior Draws
summary(seg.bf.chain)
#Examine the chain object
head(seg.bf.chain)
seg.bf.chain[1:4, 1:5]
seg.bf.chain[1:4,2:5]+seg.bf.chain[1:4,1]
seg.bf.chain.total<-seg.bf.chain[,2:5]+seg.bf.chain[,1]
seg.bf.ci<-t(apply(seg.bf.chain.total,2,quantile, pr=c(0.025,0.5,0.975)))
seg.bf.ci

#6.6.4
#Plotting the Bayesian Credible Intervals
library(ggplot2)
seg.bf.df<-data.frame(seg.bf.ci)
seg.bf.df$Segment<-rownames(seg.bf.df)
p<-ggplot(seg.bf.df, aes(x=Segment, y=X50., ymax=X97.5., ymin=X2.5,))
p<-p+geom_point(size=4)+geom_errorbar(width=0.2)+ylab("Income")
p+ggtitle("95% CI for Mean Income by Segment")+coord_flip()
