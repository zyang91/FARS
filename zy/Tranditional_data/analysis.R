library(duckdb)
setwd("/Users/apple/Desktop")
con <-dbConnect(duckdb::duckdb(), "crash.duckdb")
csv_path <- "/Users/apple/Desktop/person2022.csv"
dbExecute(con, sprintf("CREATE TABLE person2022 AS SELECT * FROM read_csv_auto('%s')", csv_path))

dbListTables(con)

data<- dbGetQuery(con, "SELECT * FROM person2022 LIMIT 5")
str(data)

analysis <- dbGetQuery(con, "SELECT STATE,ST_case, COUNTY, AGE FROM person2022")

