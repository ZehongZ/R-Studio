library(shiny)
library(DBI)
library(RMySQL)

server <- function(input, output, session) {
  output$tbl<-renderTable({
    conn<-dbConnect(
      drv=RMySQL::MySQL(),
      dbname="shinydemo",
      host="shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
      username="guest",
      password="guest")
    on.exit(dbDisconnect(conn),add=TRUE)
    dbGetQuery(conn, paste0(
      "SELECT* FROM City LIMIT ",input$nrow,";"
    ))
  })
  
}
