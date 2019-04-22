library(DBI)
library(pool)
library(RMySQL)
library(shiny)

conn<-DBI::dbConnect(
  drv=RMySQL::MySQL(),
  dbname="shinydemo",
  host="shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username="guest",
  password="guest"
)

server <- function(input, output, session) {
  output$tbl<-renderTable({
    sql<-"SELECT* FROM City WHERE ID=?id;"
    query<-sqlInterpolate(conn, sql, id=input$ID)
    dbGetQuery(conn,query)
  })
  output$popPlot<-renderPlot({
    query<-paste0("SELECT* FROM City LIMIT ",
                  as.integer(input$nrow)[1],";")
    df<-dbGetQuery(conn, query)
    pop<-df$Population
    names(pop)<-df$Name
    barplot(pop)
  })
  
}
