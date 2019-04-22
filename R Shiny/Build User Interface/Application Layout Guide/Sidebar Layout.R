library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs","Number of observations:",min=1, max=1000, value=500)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui=ui, server=server)