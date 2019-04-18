library(shiny)

#fluidPage is used to create a display that automatically adjusts to the dimensions of your user's browser window
#You lay out the user interface of your app by placing elements in the fluidpage

#sidebarPanel function output
#mainPanel function output

#HTML CONTENT
#You can add content to your Shiny app by placing it inside a *Panel function
#Use Shiny'HTML_tag functions to add more advanced content such as: h1("My Title")

#FORMAT TEXT
#p:creates a paragraph of text
#strong():makes bold text
#em():creates italicized text
#code():code displays your text similar to computer code
#div():creates segments of text
#span():span does the same thing as div, but it works with groups of words that appear inside a paragraph

#IMAGE
#Image must be put in www folder

#Define UI
ui<-fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      img(src="rstudio.png",height=140, width=400)
    )
  )
)
