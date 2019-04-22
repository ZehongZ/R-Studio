library(DBI)
library(RMySQL)
library(shiny)
library(pool)

ui <- fluidPage(
  textInput("ID","Enter your ID:","5"),
  tableOutput("tbl"),
  numericInput("nrows","How many cities to show?",10),
  plotOutput("popPlot")
)
