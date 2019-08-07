library(dplyr)
credit<-read.csv("credit.csv")
credit_tbl<-as.tbl(credit)
credit_tbl

#Connect to a database
credit_db_conn<-src_sqlite("credit.sqlite3", create=TRUE)
#Load the data into the database, data is still in the credit.sqlite3 file
copy_to(credit_db_conn, credit_tbl, temporary = FALSE)
#Access the file and create a tbl object
credit_db_conn<-src_sqlite("credit.sqlite3")
credit_tbl<-tbl(credit_db_conn,"credit_tbl")

#Using data.table
library(data.table)
credit<-fread("credit.csv")
credit[credit_history=="good", mean(na.omit(amount))]

#Using ff format
library(ff)
credit<-read.csv.ffdf(file="credit.csv", header=TRUE)
#Using ffbase to execute functions
library(ffbase)
mean(credit$amount)

#Measure execution time
system.time(rnorm(1000000))

#Working in parallel with multicore and snow
library(parallel)
detectCores()
system.time(11<-rnorm(1000000))
system.time(12<-unlist(mclapply(1:2, function(x){
  rnorm(500000)
}, mc.cores = 2)))
library(snow)
cl1<-makeCluster(4)
#Chekc whether the clustre is operational
clusterCall(cl1, function(){Sys.info()['nodename']})
#Supply cluster with a unique parameter so they can run a different command
clusterApply(cl1, c("A","B","C","D"), function(x){paste("Cluster",x,"ready!")})
#Terminate the processes
stopCluster(cl1)

#Using foreach for parallel computing
system.time(11<-rnorm(1000000))
library(foreach)#Run in serial still
system.time(14<-foreach(i=1:4,.combine = "c") %do% rnorm(250000))
library(doParallel)#Now run in parallel
registerDoParallel(cores=4)
stopImplicitCluster()#Close doParallel cluster

#Training and evaluating models in parallel with caret
library(caret)
credit<-read.csv("credit.csv")
system.time(train(default~., data=credit, method="rf"))

library(doParallel)
registerDoParallel(cores=2)
system.time(train(default~., data=credit, method="rf"))
stopImplicitCluster()
