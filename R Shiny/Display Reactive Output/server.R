library(shiny)

#Tell Shiny how to build the object

#The server function plays a special role in the Shiny process
#It builds a list-like object named output that contains all of the code needed to update the R objects in your app. 
#Each R object needs to have its own entry in the list

server <- function(input, output) {
  output$selected_var<-renderText({
    paste("You have selected this", input$var)
  })
  
}

#Each entry to output should contain the output of one of Shiny's render functiolns.
#This function capture an R expression and do some light pre-processing on the expression.


