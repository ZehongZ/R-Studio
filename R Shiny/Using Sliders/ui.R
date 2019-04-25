library(shiny)

ui <- fluidPage(
  #App title
  titlePanel("Sliders"),
  
  #Sidebar layout with input and output definitions
  sidebarLayout(
    #Sidebar to demonstrate various slider options
    sidebarPanel(
      #Input: Simple integer interval
      sliderInput("integer","Integer:",
                  min=0, max=1000,
                  value=500),
      sliderInput("decimal","Decimal:",
                  min=0, max=1,
                  value=5.0, step=0.1),
      sliderInput("range","Range:",
                  min=1, max=1000,
                  value=c(200,500)),
      sliderInput("format","Custom Format:",
                  min=0, max=10000,
                  value=0, step=2500,
                  pre="$", sep = ",",
                  animate=TRUE),
      sliderInput("animation", "Looping Animation:",
                  min=1, max=2000,
                  value=1, step=10,
                  animate=animationOptions(interval=300, loop=TRUE))
    ),
    mainPanel(
      tableOutput("values")
    )
  )
  
)

