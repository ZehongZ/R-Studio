#Descriptive statistics
myvars<-c("mpg","hp","wt")
head(mtcars[myvars])
#Descriptive statistics via summary()
myvars<-c('mpg','hp','wt')
summary(mtcars[myvars])
#Descriptive statistics via sapply()
mystats<-function(x, na.omit=FALSE) {
  if (na.omit)
    x<-x[!is.na(x)]
  m<-mean(x)
  n<-length(x)
  s<-sd(x)
  skew<-sum((x-m)^3/s^3)/n
  kurt<-sum((x-m)^4/s^4)
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}
myvars<-c('mpg','hp','wt')
sapply(mtcars[myvars], mystats)
#Descriptive statistics via describe() in the Hmisc package
library(Hmisc)
myvars<-c('mpg','hp','wt')
describe(mtcars[myvars])
#Descriptive statistics via stat.desc() in the pastecs packages
library(pastecs)
myvars<-c("mpg","hp",'wt')
stat.desc(mtcars[myvars])
#Descriptive statistics via describe() in the psych package
library(psych)
myvars<-c("mpg","hp","wt")
describe(mtcars[myvars])
#Descriptive statistics by group
myvars<-c("mpg","hp","wt")
aggregate(mtcars[myvars],by=list(am=mtcars$am), mean)
aggregate(mtcars[myvars], by=list(am=mtcars$am),sd)
#Descriptive statistics by group using by()
dstats<-function(x) sapply(x, mystats)
myvars<-c("mpg","hp","wt")
by(mtcars[myvars], mtcars$am, dstats)
#Additional methods by group
#Summary statistics by group using summaryBy() in the doBy package
library(doBy)
summaryBy(mpg+hp+wt~am, data=mtcars, FUN=mystats)
#Summary statistics by group using describe.by() in the psych package
library(psych)
myvars<-c("mpg","hp","wt")
describeBy(mtcars[myvars],list(am=mtcars$am))

#Frequency and contingency tables
#Generating frequency tables
library(vcd)
head(Arthritis)
#One-way Tables
mytable<-with(Arthritis, table(Improved))
mytable
#Turn frequencies into proportions
prop.table(mytable)
#Turn frequencies into percentage
prop.table(mytable)*100
#Two-way Tables
mytable<-xtabs(~Treatment+Improved, data=Arthritis)
mytable
#Marginal frequencies table
margin.table(mytable,1)
#Proportional table
prop.table(mytable,1)
#Column sums and column proportions
margin.table(mytable,2)
prop.table(mytable,2)
#Add marginal sums
addmargins(mytable)
addmargins(prop.table(mytable))
#Add a Sum column alone
addmargins(prop.table(mytable,1),2)
#Add a  Sum row
addmargins(prop.table(mytable,2),1)
#Two-way Table Using CrossTable
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)
#Three-way Contingency Table
mytable<-xtabs(~Treatment+Sex+Improved, data=Arthritis)
mytable
ftable(mytable)
margin.table(mytable,1)
margin.table(mytable,2)
margin.table(mytable,3)
margin.table(mytable,c(1,3))
ftable(prop.table(mytable,c(1,2)))
ftable(addmargins(prop.table(mytable, c(1,2)),3))

#Tests of Independence
#Chi-squre test of Independence
library(vcd)
mytable<-xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)
mytable<-xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)
#Fisher's Exact Test
mytable<-xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)
#Cochran-Mantel-Haenszel Test
mytable<-xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)

#Measures of association for a two-way table
library(vcd)
mytable<-xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)

#correlations
states<-state.x77[,1:6]
cov(states)
cor(states)
cor(states, method='spearman')
#Nonsquae matrices
x<-states[,c("Population","Income","Illiteracy","HS Grad")]
y<-states[,c("Life Exp","Murder")]
cor(x,y)

#Partial Correlations
library(ggm)
colnames(states)
pcor(c(1,5,2,6), cov(states))

#Testing correlations for significance
cor.test(states[,3], states[,5])#Only for one pair
library(psych)#For group
corr.test(states, use="complete")

#Independent T-test
library(MASS)
t.test(Prob~So, data=UScrime)

#Dependent T-test
library(MASS)
sapply(UScrime[c("U1","U2")], function(x) (c(mean=mean(x),sd=sd(x))))

#Wilcoxon rank sum test
with(UScrime, by(Prob, So, median))
wilcox.test(Prob~So, data=UScrime)
#Nonparametric Wilcoxon rank sum test
sapply(UScrime[c("U1","U2")], median)
with(UScrime, wilcox.test(U1,U2, paired=TRUE))

#Kurskal-Wallis
states<-data.frame(state.region, state.x77)
kruskal.test(Illiteracy~state.region, data=states)

#Nonparametric multiple comparisons
source("http://www.statmethods.net/RiA/wmc.txt")
states<-data.frame(state.region, state.x77)
wmc(Illiteracy~state.region, data=states, method="holm")
