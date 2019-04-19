#Add control widgets

#Widget: A web element that your uses can interact with. Widgets provide a way for your users to send messages to the Shiny app.
#Shiny widgets collect a value from your user. When a user changes the widget, the value will change as well.

#To add a widget, place a widget function in sidebarPanel or mainPanel in ui object

#Each widget function requres several arguments.
#a name for the widget: The user will not see this name, you can use it access the widget's value. 
#a label: This label will appear with the widget in your app. It should be a character string but can be empty string as well
#Others argement: inititial values, ranges and increments

library(shiny)

ui<-fluidPage(
  titlePanel("Basic widgets"),
  fluidRow(
    column(3,
           h3("Buttons"),
           actionButton("action","Action"),
           br(),
           br(),
           submitButton("Submit")),
    column(3,
           h3("Single check"),
           checkboxInput("checkbox","Choice A",value=TRUE)),
    column(3,
           checkboxGroupInput("checkGroup",
                              h3("Checkbox group"),
                              choices=list("Choice 1"=1,
                                           "Choice 2"=2,
                                           "Choice 3"=3),
                              selected = 1)),
    column(3,
           dateInput("date",
                     h3("Date input"),
                     value="2014-01-01"))
  ),
  
  fluidRow(
    column(3,
           dateRangeInput("dates",h3("Date range"))),
    column(3,
           h3("Help text"),
           helpText("Note: help text isn't a true widget,",
                    "but it provides an easy way to add text to",
                    "accompany other widgets.")),
    column(3,
           numericInput("num",
                        h3("Numeric input"),
                        value=1))
  ),
  fluidRow(
    column(3,
           radioButtons("radio",h3("Radio buttons"),
                        choices=list("Choice 1"=1,"choice 2"=2,
                                     "choice 3"=3),selected = 1)),
    column(3,
           selectizeInput("select",h3("Select box"),
                          choices=list("Choice 1"=1,
                                       "Choice 2"=2,
                                       "Choice 3"=3),
                          selected=1)),
    column(3,
           sliderInput("slider1",h3("Sliders"),
                       min=0, max=100, value=50),
           sliderInput("slider2","",
                       min=0, max=100, value=c(25,75))
           ),
    column(3,
           textInput("text",h3("Text input"),
                     value="Enter text..."))
  )
)