####Understanding association rules####
#Association rules: specify patterns found in the relationships among items in itemsets.
#Association rules are always composed from subsets of itemsets and are denoted by relating one itemset on the left-hand side of th erule to another itemset on the right-hand side of the rule
#The left-hand side is the condition that needs to be met in order to trigger the rule and the right-hand side is the expected result of meeting that condition
#Association rule learners are unsupervised. The program is simply unleashed on a dataset in the hope that interesting associations are found. 

####The Apriori algorithm for association rule learning####
#Ignoring rare combination to simply
#Apriori property: all subsets of a frequent itemset must also be frequent

####Measuring rule interest--support and confidence####
#Support: measures how frequently it occurs in the data
#Confidence: A measurement of its predictive power or accuracy
#Stong rules: both high support and confidence

####Identifying frequently purchased groceries with association rules
#Sparse matrix: It only stores the cells that are occupied by an item. It does not store the full matrix matrix in memory since there is no benefit to storing all these zero values
library(arules)
groceries<-read.transactions("groceries.csv",sep=",")
summary(groceries)
inspect(groceries[1:5])
itemFrequency(groceries[,1:3])
#Visualizing item support
itemFrequencyPlot(groceries,support=0.1)#at least 10 percent support
itemFrequencyPlot(groceries, topN=20)
#Visualizing the transaction data
image(groceries[1:5])
image(sample(groceries,100))
#Train a model on the data
groceryrules<-apriori(groceries, parameter = list(support=0.006, confidence=0.25, minlen=2))
groceryrules
summary(groceryrules)
inspect(groceryrules[1:3])
#Improving model performance
inspect(sort(groceryrules, by="lift")[1:5])
#Taking subsets of association rules
berryrules<-subset(groceryrules, items%in% "berries")
inspect(berryrules)
#Visualizing association rules
library(arulesViz)
plot(berryrules)
plot(groceryrules,method="grouped")
plot(groceryrules, method="graph")
