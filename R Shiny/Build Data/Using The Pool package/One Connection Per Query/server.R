library(DBI)
library(RMySQL)
library(shiny)
library(pool)

args<-list(
  drv=RMySQL::MySQL(),
  dbname="shinydemo",
  host="shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  usename="guest",
  password="guest"
)

server <- function(input, output, session) {
  output$tbl<-renderTable({
    conn<-do.call(DBI::dbConnect, args)
    on.exit(DBI::dbDisconnect(conn))
    sql<-"SELECT* FROM City WHERE ID = ?id;"
    query<-sqlInterpolate(conn, sql, id=input$ID)
    dbGetQuery(conn, query)
  })
  output$popPlot<-renderPlot({
    conn<-do.call(DBI::dbConnect,args)
    on.exit(DBI::dbDisconnect(conn))
    query<-paste0("SELECT* FROM City LIMIT ",
                  as.integer(input$nrows)[1],";")
    df<-dbGetQuery(conn, query)
    pop<-df$Population
    names(pop)<-df$Name
    barplot(pop)
  })
  
}

