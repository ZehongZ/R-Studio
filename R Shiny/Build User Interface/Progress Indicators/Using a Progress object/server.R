library(shiny)

server <- function(input, output) {
  output$plot<-renderPlot({
    input$goPlot#Re-run with button is cliked
    
    #Create 0-row data frame which will be used to store data
    dat<-data.frame(x=numeric(0), y=numeric(0))
    
    #Create a Progress object
    progress<-shiny::Progress$new()
    #Make sure it closes when we exit this reactive
    on.exit(progress$close())
    progress$set(message="Making plot",value=0)
    
    #Number of times we'll go through the loop
    n<-10
    for (i in 1:n){
      dat<-rbind(dat, data.frame(x=rnorm(1),y=rnorm(1)))
      
      #Increment the progress bar, and update the detail text
      progress$inc(1/n, detail=paste("Doing part",i))
      
      #Pause for 0.1 seconds to simulate a long computation
      Sys.sleep(0.1)
    }
    plot(dat$x,dat$y)
  })
  
}
