library(shiny)

server <- function(input, output) {
  sliderValues<-reactive({
    data.frame(
      Name=c("Integer","Decimal","Range","Custom Format","Animation"),
      value=as.character(c(input$integer,input$decimal,paste(input$range, collapse = " "),
                           input$format,
                           input$animation)),
      stringsAsFactors = FALSE
    )
  })
  output$values<-renderTable({
    sliderValues()
  })
}
