#9.1 Handling Highly Correlated Variables
cust.df <- read.csv("http://goo.gl/PmPkaG")
summary(cust.df)
str(cust.df)
#9.1.1 An Initial Linear Model of Online Spend
spend.ml<-lm(online.spend~., data=subset(cust.df[,-1], online.spend>0))
summary(spend.ml)
#Visualizing 
library(gpairs)
gpairs(cust.df)
#Box-Cox transformation
autoTransform<-function(x){
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}
cust.df.bc<-cust.df[complete.cases(cust.df),-1]
cust.df.bc<-subset(cust.df.bc, online.spend>0)
numcols<-which(colnames(cust.df.bc)!="email")
cust.df.bc[,numcols]<-lapply(cust.df.bc[,numcols], autoTransform)
summary(cust.df.bc)
#Refit the model using the transformed data
spend.m2<-lm(online.spend~., data=cust.df.bc)
summary(spend.m2)
spend.m3<-lm(online.spend~online.trans, data=cust.df.bc)
anova(spend.m3, spend.m2)

#9.1.2 Remediating Collinearity
library(car)
vif(spend.m2)
#Excluding correlated variables
spend.m4<-lm(online.spend~.,-online.trans -store.trans,data=cust.df.bc)
vif(spend.m4)
summary(spend.m4)
#PCA to extract the component 
pc.online<-prcomp(cust.df.bc[, c("online.visits", "online.trans")])
cust.df.bc$online<-pc.online$x[,1]
pc.store<-prcomp(cust.df.bc[,c("store.trans", "store.spend")])
cust.df.bc$store<-pc.store$x[,1]
#Fit a new model
spend.m5<-lm(online.spend~email+age+credit.score+distance.to.store+sat.service+
               sat.selection+online+store, data=cust.df.bc)
summary(spend.m5)
vif(spend.m5)

#9.2.1
#Basics of the Logistic Regression Model
pass.df <- read.csv("http://goo.gl/J8MH6A")
str(pass.df)
pass.df$Promo<-factor(pass.df$Promo, levels=c("NoBundle", "Bundle"))
summary(pass.df)
str(pass.df)
#Expand the table to a data frame
table(pass.df$Pass, pass.df$Promo)
pass.df$Promo<-factor(pass.df$Promo, levels = c("NoBundle","Bundle"))
table(pass.df$Pass, pass.df$Promo)

#9.2.6
#Fitting a Logistic Regression Model
pass.m1<-glm(Pass~Promo, data=pass.df, family = binomial())
summary(pass.m1)
plogis(0.3888)/(1-plogis(0.3888))
exp(0.3888)
exp(coef(pass.m1))
exp(confint(pass.m1))

#9.2.7
#Reconsidering the Model
table(pass.df$Pass, pass.df$Channel)
library(vcd)
doubledecker(table(pass.df))
pass.m2<-glm(Pass~Promo+Channel, data=pass.df, family=binomial)
summary(pass.m2)
exp(coef(pass.m2))
exp(confint(pass.m2))
pass.m3<-glm(Pass~Promo+Channel+Promo:Channel,
             data=pass.df, family = binomial())
summary(pass.m3)
exp(confint(pass.m3))

#9.3.2
#Ratings-Based Conjoint Analysis for the Amusement Park
conjoint.df <- read.csv("http://goo.gl/G8knGV")
conjoint.df$speed<-factor(conjoint.df$speed)
conjoint.df$height<-factor(conjoint.df$height)
summary(conjoint.df)

#9.3.4
#An Initial Linear Model
by(conjoint.df$rating, conjoint.df$height, mean)
ride.lm<-lm(rating~speed+height+const+theme, data=conjoint.df)
summary(ride.lm)

#9.4.5
#Hierarchical Linear Model with lme4
library(lme4)
ride.hlm1<-lmer(rating~speed+height+const+theme+(1|resp.id),data=conjoint.df)
summary(ride.hlm1)
fixef(ride.hlm1)
head(ranef(ride.hlm1)$resp.id)
head(coef(ride.hlm1)$resp.id)

#9.3.6
#The Complete Hierarchical Linear Model
ride.hlm2<-lmer(rating~speed+height+const+theme+
                  (speed+height+const+theme | resp.id),
                data=conjoint.df,
                control=lmerControl(optCtrl = list(maxfun=100000)))
fixef(ride.hlm2)
head(ranef(ride.hlm2)$resp.id)
head(coef(ride.hlm2)$resp.id)
fixef(ride.hlm2)+ranef(ride.hlm2)$resp.id[196,]
coef(ride.hlm2)$resp.id[196,]
