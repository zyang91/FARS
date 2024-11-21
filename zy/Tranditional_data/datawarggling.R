library(duckdb)
library(tidyverse)
setwd("C:/Users/zyang/OneDrive/Desktop/FARS/zy/Tranditional_data")
con <- dbConnect(duckdb::duckdb(), dbdir = "raw_data.duckdb")

dbListTables(con)

FARS2022<- dbGetQuery(con, "SELECT * FROM person2022")

#close connection
dbDisconnect(con, shutdown = TRUE)

#selection of useful variable
FARS2022_filter<- FARS2022 %>% 
  select(ST_CASE,
         STATE,
         STATENAME,
         COUNTY,
         AGE,
         AGENAME,
         SEX,
         INJ_SEV,
         INJ_SEVNAME)

## Notes: 1. connect duckdb database, 2. bring in the whole table into R varaibles
## 3. select useful variables using tidyverse(dplyr) package 
## 4. close the connection after finished
## Please do not write any additional table into this dataset. It will be easier to keep only raw data into this one.







#close connection
dbDisconnect(con, shutdown = TRUE)


## No age data
FARS2022_noage<- FARS2022 %>% 
  filter(AGE==999|AGE==998)%>%
  select(STATE,AGE,COUNTY)