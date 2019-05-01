library(shiny)

ui <- basicPage(
  plotOutput("plot1",click="plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output, session) {
  output$plot1<-renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
  output$info<-renderPrint({
    nearPoints(mtcars, input$plot_click, xvar="wt", yvar="mpg")
  })
}

shinyApp(ui, server)