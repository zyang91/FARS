library(duckdb)
setwd("/Users/apple/Desktop")
con <-dbConnect(duckdb::duckdb(), "crash.duckdb")
csv_path2022 <- "/Users/apple/Desktop/person2022.csv"
dbExecute(con, sprintf("CREATE TABLE person2022 AS SELECT * FROM read_csv_auto('%s')", csv_path2022))

dbListTables(con)

data<- dbGetQuery(con, "SELECT * FROM person2022 LIMIT 5")
str(data)


csv_path2021 <- "/Users/apple/Desktop/person2021.csv"
dbExecute(con, sprintf("CREATE TABLE person2021 AS SELECT * FROM read_csv_auto('%s')", csv_path2021))


csv_path2019 <- "/Users/apple/Desktop/person2019.csv"
dbExecute(con, sprintf("CREATE TABLE person2019 AS SELECT * FROM read_csv_auto('%s')", csv_path2019))

csv_path2020 <- "/Users/apple/Desktop/person2020.csv"
dbExecute(con, sprintf("CREATE TABLE person2020 AS SELECT * FROM read_csv_auto('%s')", csv_path2020))
dbListTables(con)

analysis <- dbGetQuery(con, "SELECT STATE,ST_case, COUNTY, AGE FROM person2022")

