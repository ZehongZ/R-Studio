#Import libraries
library(dplyr)
library(readxl)
#Import dataset
df<-read_excel("Online Retail.xlsx", sheet="Online Retail")
#Overview of the dataset
summary(df)
str(df)
#Ignore cancel orders
df<-df[which(df$Quantity>0),]
#Handling NA values
sum(is.na(df$CustomerID))
#Look at the records with no customer id
head(df[which(is.na(df$CustomerID)),])
#Remove records with NA
df<-na.omit(df)
#Currrent DataFrame shape
dim(df)
#Building a customer-item matrix
library(reshape2)
customerItemMatrix<-dcast(df, CustomerID~StockCode, value.var="Quantity")
customerItemMatrix
#0-1 encode
encode_fn<-function(x) {as.integer(x>0)}
customerItemMatrix<-customerItemMatrix%>%
  mutate_at(vars(-CustomerID), funs(encode_fn))
#Collaborative Filtering
library(coop)
###User-based collaborative filtering and recommmendations
#User-to-User Similarity Matrix
userToUserSimMatrix<-cosine(
  as.matrix(
    t(customerItemMatrix[, 2:dim(customerItemMatrix)[2]])
  )
)
colnames(userToUserSimMatrix)<-customerItemMatrix$CustomerID
top10SimilarCustomersTo12350<-customerItemMatrix$CustomerID[
  order(userToUserSimMatrix[,"12350"], decreasing = TRUE)[1:11]
]
top10SimilarCustomersTo12350
#Retrieve the items that customer 12350 has purchased in the past
itemsBoughtByA<-customerItemMatrix[
  which(customerItemMatrix$CustomerID=="12350"),
]
itemsBoughtByA<-colnames(customerItemMatrix)[which(itemsBoughtByA!=0)]
itemsBoughtByA
itemsBoughtByB<-customerItemMatrix[
  which(customerItemMatrix$CustomerID=="17935"),
]
itemsBoughtByB<-colnames(customerItemMatrix[which(itemsBoughtByB!=0)])
itemsBoughtByB
itemsToRecommendToB<-setdiff(itemsBoughtByA, itemsBoughtByB)
itemsToRecommendToB
#Get the descriptions of these items
itemsToRecommendToBDescriptions<-unique(
  df[
    which(df$StockCode%in%itemsToRecommendToB),
    c("StockCode","Description")
  ]
)
itemsToRecommendToBDescriptions<-itemsToRecommendToBDescriptions[
  match(itemsToRecommendToB, itemsToRecommendToBDescriptions$StockCode),
]
###Item-based collaborative filtering and recommendations
itemToItemSimMatrix<-cosine(
  as.matrix(
    customerItemMatrix[,2:dim(customerItemMatrix)[2]]
  )
)
top10SimilarItemsTo23166<-colnames(itemToItemSimMatrix)[
  order(itemToItemSimMatrix[,"23166"], decreasing = TRUE)[1:11]
]
top10SimilarItemsTo23166
#Get the descriptions of these similar items
top10SimilarItemDescriptions<-unique(
  df[
    which(df$StockCode%in%top10SimilarItemsTo23166),
    c("StockCode", "Description")
  ]
)
top10SimilarItemDescriptions<-top10SimilarItemDescriptions[
  match(top10SimilarItemsTo23166, top10SimilarItemDescriptions$StockCode),
]
