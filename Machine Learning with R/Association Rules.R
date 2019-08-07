#Import dataset
library(arules)
groceries<-read.transactions("groceries.csv",sep=",")

#Overview of dataset
summary(groceries)

#Examining transaction
inspect(groceries[1:5])
itemFrequency(groceries[,1:3])

#Visualizing item support
itemFrequencyPlot(groceries, support=0.1)
itemFrequencyPlot(groceries, topN=20)

#Plotting the sparse matrix
image(groceries[1:5])
image(sample(groceries,100))

#Training a model
apriori(groceries)
groceryrules<-apriori(groceries, parameter = list(support=0.006, confidence=0.25, minlen=2))
groceryrules

#Evaluting model performance
summary(groceryrules)
inspect(groceryrules[1:3])#higher lyft values indicate more importance

#Improving model performance
#Sorting the st of association rules
inspect(sort(groceryrules, by="lift")[1:5])

#Taking subsets of association rules
berryrules<-subset(groceryrules, items %in% "berries")
inspect(berryrules)
