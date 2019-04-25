library(shiny)

ui <- fluidPage(
  plotOutput('plot', width="300px", height="300px"),
  actionButton('goPlot','Go plot')
)
