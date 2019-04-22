library(shiny)

ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  fluidRow(
    column(4,
           wellPanel(
             sliderInput("obs","Number of observations:",
                         min=1, max=1000, value=5000)
           )),
    column(8,
           plotOutput("disPlot"))
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)