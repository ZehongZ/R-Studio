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

#Define UI
ui<-fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      p("p creates a paragraph of text."),
      p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragrah.", style="font-family:'times';fonrt-si16pt"),
      strong("strong() makes bold text."),
      em("em() creates italicized text."),
      br(),
      code("code displays your text similar to computer code"),
      div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style=color:blue' to div",style="color:blue"),
      br(),
      p("span does the same thing as div, but it works with",
        span("groups of works",style="color:blue"),
        "that appear inside a paragraph.")
    )
  )
)
