library(shiny)

server <- function(input, output) {
  output$mytable=DT::renderDataTable({
    mtcars
  })
  
}
