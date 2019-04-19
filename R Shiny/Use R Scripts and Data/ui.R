library(shiny)

#The lession will show you how to load data, R Scripts and packages to use in your Shiny apps.
#Create a folder data and move data source in it

#The app function is run onece, when you launch your app
#The server function is run once each time a user visits your app
#The R expression inside render functions are run many times. Shiny runs them once each time a user change the value of a widget.

#Source scrips, load libraries, and read data sets at the beginning of app.R outside of the server function
#Define user specific objects inside server function, but outside of any render calls. These would be objects that you think each user will need their own personal copy of
#Only place code that Shiny must rerun to build an object inside of a render function. Shiny will rerun all of the code in a render chunk each time a user changes a widget mentioned in the chunk

ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with information from the 2010 US census."),
      
      selectInput("var",
                  label="Choose a variable to display",
                  choices=c("Percent White",
                            "Percent Black",
                            "Percent Hispanic",
                            "Percent Asian"),
                  selected = "Percent White"),
      sliderInput("range",
                  label="Range of interest:",
                  min=0, max=100, value=c(0,100))
    ),
    mainPanel = (plotOutput("map"))
  )
  
)

