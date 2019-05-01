library(shiny)

ui <- basicPage(
  plotOutput("plot1",click = "plot_click"),
  verbatimTextOutput("info")
)
