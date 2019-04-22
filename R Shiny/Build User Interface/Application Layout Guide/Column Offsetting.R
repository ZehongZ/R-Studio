library(ggplot2)
library(shiny)

ui <- fluidPage(
  fluidRow(
    column(4,"4"),
    column(4, offset = 4, "4 offset 4")
  ),
  fluidRow(
    column(3, offset=3,
           "3 offset 3"),
    column(3, offset=3,"3 offset 3")
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)