library(shiny)
library(DBI)
library(shiny)

ui <- fluidPage(
  numericInput("nrow","Enter the number of rows to display:",5),
  tableOutput("tbl")
  
)
