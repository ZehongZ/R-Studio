#Import data
library(data.table)
train=data.table::fread("train.csv",header = T)

#Missing Value Plots
library(VIM)
aggr(train, numbers=TRUE, prop=c(TRUE, FALSE))
