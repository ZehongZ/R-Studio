library(shiny)

ui <- fluidPage(
  selectInput("dataset","Dataset",c("diamonds","rock","pressure","cars")),
  conditionalPanel(condition="output.nrows",
                   checkboxInput("headonly","Only use first 1000 rows"))
  
)

