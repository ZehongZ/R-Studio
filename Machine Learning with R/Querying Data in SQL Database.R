library(RODBC)
my_db<-odbcConnect("my_dsn",
                   uid="my_username",
                   pwd="my_password")
my_query<-"select* from my_table where my_value=1"
results_df<-sqlQuery(channel = my_db, query = sql_query, stringsAsFactors=FALSE)
obdcClose(my_db)

library(sqldf)
data("iris")
str(iris)
head(iris)
sqldf('Select * FROM iris')
sqldf('SELECT "Petal.Width" FROM iris')
sqldf('SELECT* FROM iris LIMIT 5')
