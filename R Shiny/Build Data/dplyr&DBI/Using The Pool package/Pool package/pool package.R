library(shiny)
library(DBI)
library(pool)

#MOTIVATION
#Pool adds a new level of abstraction when connecting to a database: instead of directly fetching a connection from the database, you will create an object with a reference to that database
#The pool holds a number of connections to the database
#Each time you make a query, you are quering the pool rather than database
#Under the hood, the pool will either give you an idle connection that it previously fetched from the database.
#You never have to create or close connections directly.

#CONNECTION MANAGEMENT AND PERFORMANCE
library(shiny)
library(DBI)
library(pool)
library(RMySQL)
pool<-dbPool(
  drv=RMySQL::MySQL(),
  dbname="shinydemo",
  host="shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username="guest",
  password="guest"
)
