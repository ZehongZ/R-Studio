#3.11
#Import the dataset
store.df<-read.csv("store-df.csv", header = T)
#Overview of the dt
head(store.df)
str(store.df)
#Create a data simulating of sales
k.stores<-20
k.weeks<-104
store.df<-data.frame(matrix(NA, ncol=10, nrow=k.stores*k.weeks))
names(store.df)<-c("storeNum","Year","Week","plsales","p2sales", "plprice","p2price","p1prom","p2prom","country")
#Overview
dim(store.df)
str(store.df)
#Create two vectors that will represent the store number and country
store.num<-101:(100+k.stores)
(store.cty<-c(rep("US",3), rep("DE",5),rep("GB",3), rep("BR",2), rep("JP",4), rep("AU",1), rep("CN",2)))
length(store.cty)
#Replace the appropriate columns with those values, using rep to expand the vectors to match the number of stores and weeks
store.df$storeNum<-rep(store.num, each=k.weeks)
store.df$country<-rep(store.cty, each=k.weeks)
rm(store.num, store.cty)
#Same for the Week and Year Columns
(store.df$Week<-rep(1:52, times=k.stores*2))
(store.df$Year<-rep(rep(1:2, each=k.weeks/2), times=k.stores))
#Overview data set
str(store.df)
#Redefine variable
store.df$storeNum<-factor(store.df$storeNum)
store.df$country<-factor(store.df$country)
str(store.df)
set.seed(98250)

#3.12
#Simulating data points
store.df$p1prom<-rbinom(n=nrow(store.df), size=1, p=0.1)#10% promoted
store.df$p2prom<-rbinom(n=nrow(store.df), size=1, p=0.15)#15% promoted
head(store.df)
store.df$p1price<-sample(x=c(2.19,2.29,2.49,2.79,2.99), size=nrow(store.df), replace=TRUE)
store.df$p2price<-sample(x=c(2.29, 2.49, 2.59, 2.99, 3.19), size=nrow(store.df), replace=TRUE)
head(store.df)
tmp.sales1<-rpois(nrow(store.df), lambda = 120)
tmp.sales2<-rpois(nrow(store.df), lambda=100)
tmp.sales1<-tmp.sales1*log(store.df$p2price)/log(store.df$p1price)
tmp.sales2<-tmp.sales2*log(store.df$p1price)/log(store.df$p2price)
store.df$p1sales<-floor(tmp.sales1*(1+store.df$p1prom*0.3))
store.df$p2sales<-floor(tmp.sales2*(1+store.df$p2prom*0.4))
#Overview of the data frame
head(store.df)
#Insepct data
library(car)
some(store.df, 10)

#3.21
#Count of product 1
table(store.df$p1price)
p1.table<-table(store.df$p1price)
p1.table
plot(p1.table)
#Two way cross tabs
table(store.df$p1price, store.df$p1prom)
#Compute the exact fraction of times product 1 is on promotion at each price point
p1.table2<-table(store.df$p1price, store.df$p1prom)
p1.table2[,2]/(p1.table2[,1]+p1.table2[,2])

#3.22
#Distribution functions on numeric vectors
min(store.df$p1sales)
max(store.df$p2sales)
mean(store.df$p1prom)
median(store.df$p2sales)
var(store.df$plsales)
sd(store.df$p1sales)
IQR(store.df$p1sales)
mad(store.df$p1sales)
quantile(store.df$p1sales, probs=c(0.25,0.5,0.75))
#Find other quantiles
quantile(store.df$p1sales, probs = c(0.05, 0.95))
quantile(store.df$p1sales, probs=0:10/10)
#Summary of the sales
mysummary.df<-data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary.df)<-c("Median Sales","IQR")
rownames(mysummary.df)<-c("Product 1", "Product 2")
mysummary.df["Product 1", "Median Sales"]<- median(store.df$p1sales)
mysummary.df["Product 2", "Median Sales"]<- median(store.df$p2sales)
mysummary.df["Product 1", "IQR"]<-IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"]<-IQR(store.df$p2sales)
mysummary.df

#3.3
summary(store.df)

#3.32
#Describe()
library(psych)
describe(store.df)
describe(store.df[,c(2,4:9)])

#3.34
#Apply()
apply(store.df[,2:9], MARGIN = 2, FUN=mean)#Column means
apply(store.df[,2:9], MARGIN = 1, FUN=mean)#Row means
apply(store.df[,2:9], 2, sum)
apply(store.df[,2:9], 2, sd)
#Apply that calculation to multiple columns using an anonymouns function
apply(store.df[,2:9], 2, function(x) {mean(x)-median(x)})

#3.4
#Single Variable Visualization
hist(store.df$p1sales,
     main="Product 1 Weekly Sales",
     xlab="Product 1 Sales",
     ylab="Count",
     breaks=30,
     col="lightblue")
#Using relative frequencies
hist(store.df$p1sales,
     main="Product 1 Weekly Sales",
     xlab="Product 1 Sales",
     ylab="Count",
     breaks=30,
     col="lightblue",
     freq = FALSE)
#Add smoothed estimation line
lines(density(store.df$p1sales, bw=10),
      type="l", col="darkred", lwd=2)

#3.42 Boxplot
boxplot(store.df$p2sales, xlab="Weekly Sales",ylab="P2", main="Weekly Sales of P2, All stores", horizontal = TRUE)
boxplot(store.df$p2sales~store.df$storeNum, horizontal=TRUE,
        xlab="Store", xlab="Weekly unit sales", las=1,
        main="Weekly Sales of P2 by Store")
boxplot(p2sales~p2prom, data=store.df, horizontal=TRUE, yaxt="n",
        ylab="P2 promoted in store?", xlab="Weekly sales",
        main="Weekly sales of P2 with and without promotion")
axis(side=2, at=c(1,2), labels = c("No", "Yes"))

#3.43 QQ Plot to Check Normality
qqnorm(store.df$p1sales)
qqline(store.df$p2sales)
#Transform
qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))

#3.44 Cumulative Distribution
plot(ecdf(store.df$p1sales),
     main="Cumulative distribution of P1 Weekly Sales",
     ylab=c("Cumulative distribution of p1 Weekly Sales"),
     xlab=c("P1 Weekly sales, all stores"),
     yaxt="n")
axis(side=2, at=seq(0,1,by=0.1, las=1,
                    labels=paste(seq(0,100,by=10),"%", sep="")))
abline(h=0.9, lty=3)
abline(v=quantile(store.df$p1sales, pr=0.9), lty=3)
#By
by(store.df$p1sales, store.df$storeNum, mean)
by(store.df$p1sales, list(store.df$storeNum, store.df$Year),mean)
#Aggregate
aggregate(store.df$p1sales, by=list(country=store.df$country), sum)

#3.46
p1sales.sum<-aggregate(store.df$p1sales, by=list(country=store.df$country),sum)
library(rworldmap)
library(RColorBrewer)
#Associate Data with region
p1sales.map<-joinCountryData2Map(p1sales.sum, joinCode="ISO2", nameJoinColumn = "country")
#Draw the map data
mapCountryData(p1sales.map, nameColumnToPlot = "x",
               mapTitle = "Total P1 Sales by Country",
               colourPalette = brewer.pal(7, "Greens"),
               catMethod ="fixedWidth", addLegend=FALSE)
