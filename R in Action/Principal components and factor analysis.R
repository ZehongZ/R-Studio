#Principal components analysis (PCA) is a data-reduction technique that transforms a larger number of correlated variables into a much smaller set of uncorrelated variables called principal components.
#In contrast, exploratory factor analysis (EFA) is a collection of methods designed to uncover the latent structure in a given set of variables. It looks for a smaller set of underlying or latent constructs that can explain the relationships among the observed or manifest variables.
#The goal of PCA is to replace a large number of correlated variables with a smaller number of uncorrelated variables while capturing as much information in the original variables as possible.

#Principle Components analysis of USJudgeRatings
library(psych)
pc<-principal(USJudgeRatings[,-1],nfactor=1)
pc
data=USJudgeRatings
head(data[-1])
#Communalities: the amount of vairance in each vairable explained by the components

pc<-principal(Harman23.cor$cov, nfactors = 2, rotate = "none")
pc

#Scree Plot
fa.parallel(Harman23.cor$cov, n.obs=302, fa="pc", n.iter=100, show.legend = FALSE, main="Scree plot with parallel analysis")

#Principal components analysis with varimax rotation
# They do this by “purifying” the compo- nents as much as possible.
#They also differ in their definition of purifying. The most pop- ular orthogonal rotation is the varimax rotation, which attempts to purify the columns of the loading matrix, so that each component is defined by a limited set of variables
rc<-principal(Harman23.cor$cov, nfactors = 2, rotate="varimax")
rc

#Obtaining principal components score
library(psych)
pc<-principal(USJudgeRatings[,-1],nfactors = 1, score=TRUE)
head(pc$scores)

#Obtaining principal component scoring coefficients
library(psych)
rc<-principal(Harman23.cor$cov, nfactors = 2, rotate = "varimax")
round(unclass(rc$weights),2)
#Use coefficient to calculate component scores (0.28*height+0.3*arm.span)

#Exploratory Factor Analysis
options(digits=2)
covariances<-ability.cov$cov
correlations<-cov2cor(covariances)
correlations

#Deciding how many common factors to extract
library(psych)
covariances<-ability.cov$cov
correlations<-cov2cor(covariances)
fa.parallel(correlations, n.obs=112, fa="both",n.iter=100, main="Scree plots with parallel analysis")

#Principal axis factoring without rotation
fa<-fa(correlations, nfactors = 2, rotate="none", fm="pa")
fa

#Factor extraction with orthogonal rotation (Don't allow factors to correlations)
fa.varimax<-fa(correlations, nfactors=2, rotate="varimax",fm="pa")
fa.varimax

#Factor extraction with oblique rotation (Allow factors correlation)
library(GPArotation)
fa.promax<-fa(correlations, nfactors=2, rotate="promax", fm="pa")
fa.promax

fsm <- function(oblique) {
  if (class(oblique)[2]=="fa" & is.null(oblique$Phi)) {
    warning("Object doesn't look like oblique EFA")
  } else {
    P <- unclass(oblique$loading)
    F <- P %*% oblique$Phi
    colnames(F) <- c("PA1", "PA2")
    return(F)
  } 
  }
fsm(fa.promax)
fa.diagram(fa.promax,simple=FALSE)
fa.diagram(fa.promax, labels=rowname(fa.promax$loadings))

#Factor score
fa.promax$weights
