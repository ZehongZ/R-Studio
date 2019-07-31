#Exhibit 1.1
library(TSA)
data("larain")
plot(larain, ylab="Inches", xlab="Year",type='o')

#Exhibit 1.2
plot(y=larain, x=zlag(larain), ylab='Inches',xlab="Previous Year Inches")

#Exhibit 1.3
data("color")
plot(color, ylab='Color Property', xlab='Batch', type='o')

#Exhibit 1.4
plot(y=color, x=zlag(color),ylab='Color Property', xlab = 'Previous Batch Color Property')

#Exhibit 1.5
data("hare")
plot(hare, ylab='Abundance', xlab='Year', type='o')

#Exhibit 1.6
plot(y=hare, x=zlag(hare),ylab='Abundance', xlab='Previous Year Abundance')

#Exhibt 1.7
data("tempdub")
plot(tempdub, ylab="Temperature", type='o')

#Exhibit 1.8
data("oilfilters")
plot(oilfilters, type='o', ylab="Sales")

#Exhit 1.9
plot(oilfilters, type='l', ylab='Sales')
points(y=oilfilters, x=time(oilfilters), pch=as.vector(season(oilfilters)))
