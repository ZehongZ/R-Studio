#t-test
library(pwr)
pwr.t.test(d=0.8, sig.level = .05, power=.9, type="two.sample",alternative="two.sided")
#Results suggest that 34 participants are needed in each group in order to detect an effect size of 0.8 with 90% certainty
pwr.t.test(n=20, d=.5, sig.level=.01, type='two.sample',alternative="two.sided")

#ANOVA
pwr.anova.test(k=5, f=.25, sig.level = .05, power=.8)

#Correlations
pwr.r.test(r=.25, sig.level = .05, power=.9, alternative='greater')

#Linear Models
pwr.f2.test(u=3, f2=0.0769, sig.level=0.05, power=0.9)

#Tests of proportions
pwr.2p.test(h=ES.h(.65, .6), sig.level = .05, power=.9, alternative = "greater")

#Chi-square tests
prob<-matrix(c(.42,.28,.03,.07,.10,.10),byrow = TRUE, nrow=3)
ES.w2(prob)

#Sample sizes for detecting significant effects in a one-way ANOVA
library(pwr)
es<-seq(.1,.5,.01)
nes<-length(es)

samsize<-NULL
for (i in 1:nes){
  result<-pwr.anova.test(k=5, f=es[i], sig.level = .05, power=.9)
  samsize[i]<-ceiling(result$n)
}
plot(samsize, es, type="l", lwd=2, col="red",
     ylab="Effect Size",
     xlab="Sample size (per cell)",
     main="One way ANOVA with Power=.9 and Alpha=.05"
     )

#Creating power analysis plots
#Sample-size curves for detecting correlations of various sizes
library(pwr)
r<-seq(.1,.5,.01)
nr<-length(r)
p<-seq(.4,.9,.1)
np<-length(p)
samsize<-array(numeric(nr*np),dim = c(nr,np))
for (i in 1:np){
  for (j in 1:nr){
    result<-pwr.r.test(n=NULL, r=r[j],
                       sig.level=.05, power=p[i],
                       alternative = 'two.sided')
    samsize[j,i]<-ceiling(result$n)
  }
}

