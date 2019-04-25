library(shiny)

ui<-shinyUI(basicPage(
  plotOutput('plot',width="300px",height="300px"),
  actionButton('goPlot','Go plot')
))
