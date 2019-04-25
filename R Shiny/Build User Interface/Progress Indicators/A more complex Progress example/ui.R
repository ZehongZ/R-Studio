library(shiny)

ui <- shinyUI(basicPage(
  tableOutput('table'),
  actionButton('goTable','Go table')
))
