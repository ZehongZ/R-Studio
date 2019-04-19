library(shiny)

server <- function(input, output) {
  output$map<-renderPlot({
    data<-switch(input$var,
                 "Percent White"=counties$white,
                 "Percent Black"=counties$black,
                 "Percent Hispanic"=counties$hispanic,
                 "Percent Asian"=counties$asian)
    percent_map(var=data,color="blue",legend.title = "Race", max=100, min=0)
  })
}

