library(shiny)

#The simplest way to add a progress indicator is to put withProgress inside of the reactive(), observer() or renderXx() that 
#contains the long-running computation.

shinyApp(ui, server)