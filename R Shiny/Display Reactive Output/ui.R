library(shiny)

#Reactive output automatically responds when your user toggles a widget

#Place a function in ui tells Shiny where to display your object

ui <- fluidPage(
  titlePanel("censusVis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with infromation from the 2010 US Census."),
      selectInput("var",
                  label="Choose a variable to display",
                  choices=c("Percent White",
                            "Percent Black",
                            "Percent Hispanic",
                            "Percent Asian"
                            ),selected = "Percent White"),
      sliderInput("range",
                  label="Range of interest:",
                  min=0, max=100, value=c(0,100))
    ),
    mainPanel = (
      textOutput("selected_var")
    )
  )
  
)
