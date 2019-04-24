library(shiny)

server <- function(input, output) {
  #Reactive expression to generate the requested distribution
  #Whenever the inputs change, the output functions defined below then use the value computed from this expression
  d<-reactive({
    dist<-switch(input$dist, norm=rnorm,unif=runif,lnorm=rlnorm,exp=rexp,rnorm)
    dist(input$n)
  })
  
  output$plot<-renderPlot({
    dist<-input$dist
    n<-input$n
    hist(d(),
         main = paste("r",dist,"(",n,")",sep=""),
         col="#75AADB", border = "white")
  })
  
  output$summary<-renderPrint({
    summary(d())
  })
  output$table<-renderTable({
    d()
  })
}
