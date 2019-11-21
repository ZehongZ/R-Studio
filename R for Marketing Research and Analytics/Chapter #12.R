#12.2.1
#Example Data: Groceries
library(arules)
data("Groceries")
summary(Groceries)
inspect(head(Groceries,3))
#Modify search rules
groc.rules<-apriori(Groceries, parameter = list(supp=0.01, conf=0.3, target="rules"))
inspect(subset(groc.rules, lift>3))

#12.2.2
#Supermarket Data
retail.raw <- readLines("http://goo.gl/FfjDAO")
head(retail.raw)
tail(retail.raw)
summary(retail.raw)
#Split each line wherever there is a blank space character
retail.list<-strsplit(retail.raw," ")
#Label the individual transactions
names(retail.list)<-paste("Trans",1:length(retail.list), sep="")
str(retail.list)
library(car)
some(retail.list)
rm(retail.raw)
#To convert from a list to transactions
retail.trans<-as(retail.list, "transactions")
summary(retail.trans)
rm(retail.list)

#12.3
#Finding and Visualizing Association Rules
retail.rules<-apriori(retail.trans, parameter=list(supp=0.001, conf=0.4))
library(arulesViz)
plot(retail.rules, interactive = TRUE)

#12.3.1
#Finding and Plotting Subsets of Rules
retail.hi<-head(sort(retail.rules, by="lift"),50)
inspect(retail.hi)
plot(retail.hi, method="graph", control=list(type="items"))

#12.3.2
#Using Profit Margin Data with Transactions: An Initial Start
retail.itemnames<-sort(unique(unlist(as(retail.trans, "list"))))
head(retail.itemnames)
tail(retail.itemnames)
set.seed(03870)
retail.margin<-data.frame(margin=rnorm(length(retail.itemnames),
                                       mean=0.3, sd=0.3))
quantile(retail.margin$margin)
rownames(retail.margin)<-retail.itemnames
head(retail.margin)
tail(retail.margin)
library(car)
some(retail.margin)
retail.margin[c("39","48"),]
(basket.items<-as(retail.trans[3], "list")[[1]])
retail.margin[basket.items,]
sum(retail.margin[basket.items,])

#12.4
#Rules in Non-Transactional Data: Exploring Segments Again
seg.df <- read.csv("http://goo.gl/qw303p")
summary(seg.df)

#12.4.1
#Slicing Continuous Data with cut()
seg.fac<-seg.df
seg.fac$age<-cut(seg.fac$age,
                 breaks=c(0,15,35,55,65,100),
                 labels(c("19-24","25-34","35-54","55-64","65+"),
                        right=FALSE,
                        ordered_result=TRUE))
summary(seg.fac$age)
#Convert income and kids
seg.fac$income<-cut(seg.fac$income,
                    breaks = c(-100000, 40000, 70000, 1000000),
                    labels=c("Low","Medium","High"),
                    right=FALSE, 
                    ordered_result = TRUE)
seg.fac$kids<-cut(seg.fac$kids,
                  breaks=c(0,1,2,3,100),
                  labels=c("No Kids", "1 kid","2 kids","3+ kids"),
                  right = FALSE, ordered_result = TRUE)
summary(seg.fac)

#12.4.2
#Exploring Segment Associations
library(arules)
library(arulesViz)
seg.trans<-as(seg.fac, "transactions")
summary(seg.trans)
seg.rules<-apriori(seg.trans, parameter = list(support=0.1, conf=0.4, target="rules"))
summary(seg.rules)
plot(seg.rules, interactive = TRUE)
#Visualize top 35 hightest-lift rules
seg.hi<-head(sort(seg.rules, by="lift"),35)
inspect(seg.hi)
plot(seg.hi, method="graph", control=list(type="items"))
seg.next<-sort(seg.rules, by="lift")[36:60]
plot(seg.next, method="graph", control=list(type="items"))
