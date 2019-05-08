#Time Series Plot of Los Angeles Annual Rainfall
library(TSA)
data("larain")
plot(larain,ylab="Inches",xlab="Year",type="o")
#Scatterplot of LA Rainfall versus Last Year's LA Rainfall
library(TSA)
data("larain")
plot(y=larain,x=zlag(larain),ylab = "Inches",xlab = "Previous Year Inches")

#Time Series Plot of Color Property from a Chemical Process
data("color")
plot(color, xlab="Batch",ylab="Color Property",type="o")
#Scatterplot of Color Value versus Previous Color Value
plot(y=color, x=zlag(color),ylab="Color Property",xlab = "Previous Batch Color Property")

#Abundance of Canadian Hare
data("hare")
plot(hare, xlab="Year", ylab="Abundance",type="o")
#Hare Abundance versus Previous Year's Hare Abundance
plot(x=zlag(hare),y=hare,xlab="Previous Year Abundance",ylab="Abundance")

#Average Monthly Temperature, Dubuque, Iowa
data("tempdub")
plot(tempdub, ylab="Temperature",type="o")
#Seasonality for monthly values occurs when observations twelve months apart are related in some manner or another
#Models for such series must accommodate this variation while preserving the similarities
#Seasonality would be present if January values tended to be related to other January values and so forth

#Monthly Oil Filter Sales
data("oilfilters")
plot(oilfilters, type="o", ylab="Sales")
#Monthly Oil Filter Sales with Special Plotting Symbols
plot(oilfilters, type="l", ylab="Sales")
points(y=oilfilters, x=time(oilfilters),pch=as.vector(season(oilfilters)))

#Principle of parsimony: the model used should require the smaller number of parameters that will adequately represent the time series