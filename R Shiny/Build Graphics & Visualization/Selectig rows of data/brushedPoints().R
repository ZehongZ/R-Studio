library(shiny)

ui <- basicPage(
  plotOutput("plot1",brush="plot_brush"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$plot1<-renderPlot({
    plot(mtcars$wt,mtcars$mpg)
  })
  output$info<-renderPrint({
    brushedPoints(mtcars, input$plot_brush, xvar="wt",yvar="mpg")
  })
}

shinyApp(ui, server)