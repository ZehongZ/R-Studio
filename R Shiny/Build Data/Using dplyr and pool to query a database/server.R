library(DBI)
library(dplyr)
library(dbplyr)
library(pool)
library(shiny)

pool<-dbPool(
  drv=RMySQL::MySQL(),
  dbname="shinydemo",
  host="shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username="guest",
  password="guest"
)

server <- function(input, output, session) {
  output$tbl<-renderTable({
    pool%>%tbl("City")%>%
      filter(ID==input$ID)
  })
  output$popPlot<-renderPlot({
    df<-pool%>%tbl("City")%>%
      head(as.integer(input$nrows)[1])%>%collect()
    pop<-df$Population
    names(pop)<-df$Name
    barplot(pop)
  })
  
}

