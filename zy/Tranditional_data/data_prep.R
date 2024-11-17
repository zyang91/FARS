library(duckdb)
setwd("/Users/apple/Desktop")
con <-dbConnect(duckdb::duckdb(), "raw_data.duckdb")
csv_path2022 <- "/Users/apple/Desktop/person2022.csv"
dbExecute(con, sprintf("CREATE TABLE person2022 AS SELECT * FROM read_csv_auto('%s')", csv_path2022))

csv_path2021 <- "/Users/apple/Desktop/person2021.csv"
dbExecute(con, sprintf("CREATE TABLE person2021 AS SELECT * FROM read_csv_auto('%s')", csv_path2021))


csv_path2019 <- "/Users/apple/Desktop/person2019.csv"
dbExecute(con, sprintf("CREATE TABLE person2019 AS SELECT * FROM read_csv_auto('%s')", csv_path2019))

csv_path2020 <- "/Users/apple/Desktop/person2020.csv"
dbExecute(con, sprintf("CREATE TABLE person2020 AS SELECT * FROM read_csv_auto('%s')", csv_path2020))

csv_path2018 <- "/Users/apple/Desktop/person2018.csv"
dbExecute(con, sprintf("CREATE TABLE person2018 AS SELECT * FROM read_csv_auto('%s')", csv_path2018))

csv_path2017 <- "/Users/apple/Desktop/person2017.csv"
dbExecute(con, sprintf("CREATE TABLE person2017 AS SELECT * FROM read_csv_auto('%s')", csv_path2017))

csv_path2016 <- "/Users/apple/Desktop/person2016.csv"
dbExecute(con, sprintf("CREATE TABLE person2016 AS SELECT * FROM read_csv_auto('%s')", csv_path2016))

csv_path2015 <- "/Users/apple/Desktop/person2015.csv"
dbExecute(con, sprintf("CREATE TABLE person2015 AS SELECT * FROM read_csv_auto('%s')", csv_path2015))

csv_path2014 <- "/Users/apple/Desktop/person2014.csv"
dbExecute(con, sprintf("CREATE TABLE person2014 AS SELECT * FROM read_csv_auto('%s')", csv_path2014))

csv_path2013 <- "/Users/apple/Desktop/person2013.csv"
dbExecute(con, sprintf("CREATE TABLE person2013 AS SELECT * FROM read_csv_auto('%s')", csv_path2013))

csv_path2012 <- "/Users/apple/Desktop/person2012.csv"
dbExecute(con, sprintf("CREATE TABLE person2012 AS SELECT * FROM read_csv_auto('%s')", csv_path2012))

csv_path2011 <- "/Users/apple/Desktop/person2011.csv"
dbExecute(con, sprintf("CREATE TABLE person2011 AS SELECT * FROM read_csv_auto('%s')", csv_path2011))

csv_path2010 <- "/Users/apple/Desktop/person2010.csv"
dbExecute(con, sprintf("CREATE TABLE person2010 AS SELECT * FROM read_csv_auto('%s')", csv_path2010))

dbListTables(con)



analysis <- dbGetQuery(con, "SELECT STATE,ST_case, COUNTY, AGE FROM person2022")

