library(shiny)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(6,
           plotOutput("plot1", click="plot1_click")),
    column(5,
           br(), br(),br(),
           htmlOutput("x_value"),
           verbatimTextOutput("selected_rows"))
  )
)
