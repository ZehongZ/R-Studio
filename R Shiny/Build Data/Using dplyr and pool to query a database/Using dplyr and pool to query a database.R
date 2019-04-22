#Combining dplyr and pool
library(shiny)
library(DBI)
library(dplyr)
library(dbplyr)
library(pool)

#Connect to and query a MySQL database using  pool
library(dplyr)
library(RMySQL)
my_db<-dbPool(
  RMySQL::MySQL(),
  dbname="shinydemo",
  host="shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username="guest",
  password="guest"
)
#Get the first 5 rows
my_db%>%tbl("City")%>%head(5)
