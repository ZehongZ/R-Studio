library(shiny)
library(ggplot2)

server <- function(input, output, session) {
  output$plot1<-renderPlot({
    plot(ToothGrowth$supp, ToothGrowth$len)
  })
  output$x_value<-renderText({
    if (is.null(input$plot1_click$x)) return("")
    else{
      lvls<-levels(ToothGrowth$supp)
      name<-lvls[round(input$plot1_click$x)]
      HTML("You've selected <code>",name, "</code>",
           "<br><br>Here are the first 10 rows that ",
           "match that category:")
    }
  })
  #Print the rows of the data frame which match the x value
  output$selected_rows<-renderPrint({
    if (is.null(input$plot1_click$x)) return()
    else{
      keeprows<-round(input$plot1_click$x)==as.numeric(ToothGrowth$supp)
      head(ToothGrowth[keeprows,],10)
    }
  })
}

