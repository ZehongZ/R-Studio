library(shiny)

compute_data<-function(updateProgress=NULL){
  dat<-data.frame(x=numeric(0),y=numeric(0))
  
  for (i in 1:10){
    Sys.sleep(0.25)
    
    #Compute new row of data
    new_row<-data.frame(x=rnorm(1),y=rnorm(1))
    
    #If we were passed a progress update function, call it
    if(is.function(updateProgress)){
      text<-paste0("x:", round(new_row$x,2),"y:",round(new_row$y,2))
      updateProgress(detail=text)
    }
    #Add the new row of data
    dat<-rbind(dat, new_row)
  }
  dat
}

server <- function(input, output) {
  output$table<-renderTable({
    input$goTable
    #Create a Progress object
    progress<-shiny::Progress$new()
    progress$set(message="Computing data",value=0)
    on.exit(progress$close())
    
    updateProgress<-function(value=NULL, detail=NULL){
      if (is.null(value)){
        value<-progress$getValue()
        value<-value+(progress$getMax()-value)/5
      }
      progress$set(value=value, detail=detail)
    }
    compute_data(updateProgress)
  })
  
}

