library(shiny)

server <- function(input, output, session) {
  datasetInput<-reactive({
    switch(input$dataset,
           "rock"=rock,
           "pressure"=pressure,
           "cars"=cars)
  })
  output$nrows<-reactive({
    nrow(datasetInput())
  })
  outputOptions(output, "nrows", suspendWhenHidden=FALSE)
  
}
