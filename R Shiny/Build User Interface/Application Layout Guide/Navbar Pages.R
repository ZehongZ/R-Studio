library(ggplot2)
library(shiny)

ui <- navbarPage(
  "My Application",
  tabPanel("Component 1"),
  tabPanel("Component 2"),
  tabPanel("Component 3")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)