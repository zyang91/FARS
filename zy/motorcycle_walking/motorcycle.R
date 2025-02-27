library(duckdb)
library(tidyverse)
library(dplyr)
library(sf)

# Load the data
con <-dbConnect(duckdb::duckdb(), "data/raw_data.duckdb")


FARS2022<- dbGetQuery(con, "SELECT * FROM person2022")
FARS2021<- dbGetQuery(con, "SELECT * FROM person2021")
FARS2019<- dbGetQuery(con, "SELECT * FROM person2019")
FARS2018<- dbGetQuery(con, "SELECT * FROM person2018")
FARS2017<- dbGetQuery(con, "SELECT * FROM person2017")

dbDisconnect(con, shutdown = TRUE)

FARS2022<- FARS2022%>%
  filter(INJ_SEV==4)

FARS2022<- FARS2022%>%
  mutate(FIPS= sprintf("%02d%03d", FARS2022$STATE, FARS2022$COUNTY) )