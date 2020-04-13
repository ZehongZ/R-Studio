#Selecting the number of components to extract
library(psych)
fa.parallel(USJudgeRatings[,-1], fa="pc", n.iter = 100,
            show.legend = FALSE, main = "Scree plot with parallel analysis")

#Principal components analysis
library(psych)
pc<-principal(USJudgeRatings[,-1], nfactors=1)
pc

#Principal Components Analysis of Body Measurements
library(psych)
pc<-principal(Harman23.cor$cov, nfactors = 2, rotate="none")
pc

#Principal components analysis with varimax rotation
rc<-principal(Harman23.cor$cov, nfactors=2, rotate="varimax")
rc

#Obtaining component scores from raw data
library(psych)
pc<-principal(USJudgeRatings[,-1],nfactors = 1, score=TRUE)
head(pc$scores)
cor(USJudgeRatings$CONT, pc$scores)
#Obtaining principal component scoring coefficients
library(psych)
rc<-principal(Harman23.cor$cov, nfactors = 2, rotate = "varimax")
round(unclass(rc$weights),2)

#Exploratory factor analysis
options(digits = 2)
covariances<-ability.cov$cov
correlations<-cov2cor(covariances)
correlations
#Decide on the number of factors to extract
library(psych)
covariances<-ability.cov$cov
correlations<-cov2cor(covariances)
fa.parallel(correlations, n.obs=112, fa="both",n.iter = 100,
            main = "Scree Plots with parallel analysis")

#Principal axis factoring without rotation
fa<-fa(correlations, nfactors = 2, rotate="none", fm="pa")
fa
#Factor extraction with orthogonal rotation
fa.varimax<-fa(correlations, nfactors=2, rotate="varimax", fm="pa")
fa.varimax
#Factor extraction with oblique rotation
fa.promax<-fa(correlations, nfactors=2, rotate="promax",fm="pa")
fa.promax
fsm<-function(oblique){
  if (class(oblique)[2]=="fa" & is.null(oblique$phi)){
    warning("object doesn't look like oblique EFA")
  } else {
    p<-unclass(oblique$loading)
    F<-p %*% obluque$phi
    colnames(F) <- c("PAI","PAI2")
    return(F)
  }
}
fsm(fa.promax)
fa.diagram(fa.promax, simple=FALSE)
#Factor scores
fa.promax$weights
