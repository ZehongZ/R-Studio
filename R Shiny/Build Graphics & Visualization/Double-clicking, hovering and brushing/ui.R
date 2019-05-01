library(shiny)

ui <- basicPage(
  plotOutput("plot1",
             click="plot_click",
             dblclick = "plot_dblclick",
             hover="plot_hover",
             brush="plot_brush"),
  verbatimTextOutput("info")
)
