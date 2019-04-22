library(shiny)

ui <- fluidPage(
  fluidRow(
    column(12,
           "Fluid 12",
           fluidRow(
             column(6,
                    "Fluid 6",
                    fluidRow(
                      column(6,
                             "Fluid 6"),
                      column(6,
                             "Fluid 6")
                    )),
             column(width = 6,
                    "Fluid 6")
           ))
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)