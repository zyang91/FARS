library(duckdb)
library(tidyverse)
setwd("/Users/apple/FARS/zy/Tranditional_data")
con <- dbConnect(duckdb::duckdb(), dbdir = "crash.duckdb")

dbListTables(con)

FARS2022<- dbGetQuery(con, "SELECT * FROM person2022")

## No age data
FARS2022_noage<- FARS2022 %>% 
  filter(AGE==999|AGE==998)%>%
  select(STATE,AGE,COUNTY)




