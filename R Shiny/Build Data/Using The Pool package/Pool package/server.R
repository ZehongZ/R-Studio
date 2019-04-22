library(DBI)
library(pool)
library(shiny)
library(RMySQL)

pool<-dbPool(
  drv=RMySQL::MySQL(),
  dbname="shinydemo",
  host="shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username="guest",
  password="guest"
)

server <- function(input, output, session) {
  output$tbl<-renderTable({
    sql<-"SELECT*FROM City WHERE ID=?id;"
    query<-sqlInterpolate(pool, sql, id=input$ID)
    dbGetQuery(pool, query)
  })
  output$popPlot<-renderPlot({
    query<-paste0("SELECT * FROM City LIMIT ",
                  as.integer(input$nrows)[1],";")
    df<-dbGetQuery(pool,query)
    pop<-df$Population
    names(pop)<-df$Name
    barplot(pop)
  })
}


