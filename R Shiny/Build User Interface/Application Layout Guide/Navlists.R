library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Application Title"),
  navlistPanel(
    "Header A",
    tabPanel("Component 1"),
    tabPanel("Component 2"),
    "Header B",
    tabPanel("Component 3"),
    tabPanel("Component 4"),
    "------",
    tabPanel("Component 5")
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)