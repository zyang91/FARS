library(duckdb)
library(tidyverse)
setwd("C:/Users/zyang/OneDrive/Desktop/FARS/zy/Tranditional_data")
con <- dbConnect(duckdb::duckdb(), dbdir = "raw_data.duckdb")

dbListTables(con)

FARS2022<- dbGetQuery(con, "SELECT * FROM person2022")
FARS2021<- dbGetQuery(con, "SELECT * FROM person2021")
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

FARS2022_filter<- FARS2022_filter%>%
  mutate(agegroup= case_when(AGE<5~1,
                             AGE>=5 & AGE<10~2,
                             AGE>=10 & AGE<15~3,
                             AGE>=15 & AGE<19~4,
                             AGE>=19 & AGE<200~5,
                             AGE>=200~6
                             ))
#filter to only fatality
FARS2022_filter<- FARS2022_filter%>%
  filter(INJ_SEV==4)


## Function to clean FARS data

process_FARS_data <- function(data) {
  # Select relevant columns
  data_filtered <- data %>%
    select(
      ST_CASE,
      STATE,
      STATENAME,
      COUNTY,
      AGE,
      AGENAME,
      SEX,
      INJ_SEV,
      INJ_SEVNAME
    )
  
  # Add agegroup column
  data_filtered <- data_filtered %>%
    mutate(
      agegroup = case_when(
        AGE < 5 ~ 1,
        AGE >= 5 & AGE < 10 ~ 2,
        AGE >= 10 & AGE < 15 ~ 3,
        AGE >= 15 & AGE < 19 ~ 4,
        AGE >= 19 & AGE < 200 ~ 5,
        AGE >= 200 ~ 6
      )
    )
  
  # Filter rows where INJ_SEV is fatality
  data_filtered <- data_filtered %>%
    filter(INJ_SEV == 4)
  
  return(data_filtered)
}
 #test function
FARS2021_filtered <- process_FARS_data(FARS2021)


#close connection
dbDisconnect(con, shutdown = TRUE)


## No age data
FARS2022_noage<- FARS2022 %>% 
  filter(AGE==999|AGE==998)%>%
  select(STATE,AGE,COUNTY)