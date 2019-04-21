library(shiny)
data("mtcars")
names(mtcars)
mpgData<-mtcars
mpgData$am<-factor(mpgData$am,labels=c("Automatic","Manual"))


server<-function(input,output){
  formulaText<-reactive({
    paste("mpg ~",input$variable)
  })
  #Return the formula text for printing as a caption
  output$caption<-renderText({
    formulaText()
  })
  output$mpgPlot<-renderPlot({
    boxplot(as.formula(formulaText()),
            data=mpgData,
            outline = input$outliers,
            col="#75AADB", pch=19)
  })
  
}

