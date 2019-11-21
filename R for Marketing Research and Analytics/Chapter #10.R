#Import dataset
piesSimData <- read.csv("http://goo.gl/yT0XwJ")
#Overview of dataset
summary(piesSimData)

#10.2.2
#Estimating PIES CFA Model
library(lavaan)
piesModel<-"General =~i1+i2+i3
            Feature=~i4+i5+i6+i7
            Image=~i8+i9+i10+i11
            Pies=~General+Feature+Image"
pies.fit<-cfa(piesModel, data=piesSimData)
summary(pies.fit,fit.measures=TRUE)
#Visualize the model
install.packages("OpenMx")#OpenMx is not available
library(semPlot)#Require OpenMx

