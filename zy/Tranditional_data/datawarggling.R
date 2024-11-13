library(duckdb)
library(tidyverse)
setwd("/Users/apple/FARS/zy/Tranditional_data")
con <- dbConnect(duckdb::duckdb(), dbdir = "crash.duckdb")

dbListTables(con)
