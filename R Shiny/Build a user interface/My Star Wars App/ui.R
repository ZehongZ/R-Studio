library(shiny)

#fluidPage is used to create a display that automatically adjusts to the dimensions of your user's browser window
#You lay out the user interface of your app by placing elements in the fluidpage

#sidebarPanel function output
#mainPanel function output

#HTML CONTENT
#You can add content to your Shiny app by placing it inside a *Panel function
#Use Shiny'HTML_tag functions to add more advanced content such as: h1("My Title")

#Define UI
ui<- fluidPage(
  titlePanel("My Stat Wars App"),
  sidebarLayout(
    #position="right",
    sidebarPanel("sidebar panel"),
    mainPanel(
      h6("Episode IV",align="center"),
      h6("A NEW HOPE", align="center"),
      h5("It is a perid of civil war.",align="center"),
      h4("rebel spaceships, striking",align="center"),
      h3("from a hidden base, have won", align="center"),
      h2("their first victory against the",align="center"),
      h1("evil Galactic Empire")
    )
  )
)

