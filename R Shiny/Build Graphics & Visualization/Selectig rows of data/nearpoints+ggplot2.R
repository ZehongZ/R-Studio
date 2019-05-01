library(ggplot2)
library(shiny)

ui <- fluidPage(
  plotOutput("plot1",click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$plot1<-renderPlot({
    ggplot(mtcars, aes(x=wt, y=mpg))+geom_point()
  })
  output$info<-renderPrint({
    nearPoints(mtcars, input$plot_click, threshold = 10, maxpoints = 1, addDist = TRUE)
  })
}

shinyApp(ui, server)