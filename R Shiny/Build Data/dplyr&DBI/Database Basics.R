library(shiny)
library(DBI)
library(dplyr)
library(dbplyr)

#dplyr package to read data from an external database
#DBI package to hook up to an external database
#prevent SQL injections
#manage connections, prevent leaks and ensure the best performance using the pool package
#integrate the pool package with dplyr

#dplyr sample usage
library(pool)
library(dplyr)
library(RMySQL)

my_db<-dbPool(
  RMySQL::MySQL(),
  dbname="shinydemo",
  host="shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  user="guest",
  password="guest"
)
#Get the first 5 rows
my_db%>%tbl("City")%>%head(5)

#DBI Package
#connect/disconnect to the DBMS
#create and execute statements in the DBMS
#extract results/output from statements
#error/exception handling
#information from database objects
#transaction management
library(DBI)
conn<-dbConnect(
  drv=RMySQL::MySQL(),
  dbname="shinydemo",
  host="shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username="guest",
  password="guest")
rs<-dbSendQuery(conn, "SELECT*FROM City LIMIT 5;")
dbFetch(rs)

#dbGetQuery
library(DBI)
conn<-dbConnect(
  drv=RMySQL::MySQL(),
  dbname="shinydemo",
  host="shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username="guest",
  password="guest")
dbGetQuery(conn, "SELECT*FROM City LIMIT 5;")
dbDisconnect(conn)
