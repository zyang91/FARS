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


FARS2021<- FARS2021%>%
  filter(INJ_SEV==4)

FARS2022<- FARS2022%>%
  filter(INJ_SEV==4)

FARS2019<- FARS2019%>%
  filter(INJ_SEV==4)

FARS2018<- FARS2018%>%
  filter(INJ_SEV==4)

FARS2017<- FARS2017%>%
  filter(INJ_SEV==4)

FARS2022<- FARS2022%>%
  mutate(FIPS= sprintf("%02d%03d", FARS2022$STATE, FARS2022$COUNTY) )

FARS2021<- FARS2021%>%
  mutate(FIPS= sprintf("%02d%03d", FARS2021$STATE, FARS2021$COUNTY) )

FARS2019<- FARS2019%>%
  mutate(FIPS= sprintf("%02d%03d", FARS2019$STATE, FARS2019$COUNTY) )

FARS2018<- FARS2018%>%
  mutate(FIPS= sprintf("%02d%03d", FARS2018$STATE, FARS2018$COUNTY) )

FARS2017<- FARS2017%>%
  mutate(FIPS= sprintf("%02d%03d", FARS2017$STATE, FARS2017$COUNTY) )


FARS2022<- FARS2022%>%
  select(FIPS, PER_TYP, AGE)

FARS2021<- FARS2021%>%
  select(FIPS, PER_TYP, AGE)

FARS2019<- FARS2019%>%
  select(FIPS, PER_TYP, AGE)

FARS2018<- FARS2018%>%
  select(FIPS, PER_TYP, AGE)

FARS2017<- FARS2017%>%
  select(FIPS, PER_TYP, AGE)

complete<- rbind(FARS2022, FARS2021, FARS2019, FARS2018, FARS2017)

complete<- complete%>%
  mutate(age_group= case_when(
    AGE<18 ~ 1,
    AGE>=18 & AGE<200 ~ 2
  ))%>%
  filter(!is.na(age_group))

complete<- complete%>%
  select(-AGE)

complete<- complete%>%
  group_by(FIPS, age_group, PER_TYP)%>%
  summarise(count=n())


library(readxl)

cbsa<- read_excel("data/cbsa_list.xlsx") 

cbsa<- cbsa%>%
  rename(state= `FIPS State Code`,
         county= `FIPS County Code`)%>%
  rename(cbsa= `CBSA Code`,
         cbsa_name= `CBSA Title`)

top_30<-read.csv("data/together.csv")

cbsa_top30<- cbsa%>%
  filter(cbsa %in% top_30$cbsa_code)%>%
  select(cbsa, cbsa_name, state, county)

cbsa_top30$state<- as.numeric(cbsa_top30$state)
cbsa_top30$county<- as.numeric(cbsa_top30$county)

cbsa_top30$FIPS<-sprintf("%02d%03d",cbsa_top30$state, cbsa_top30$county)

complete<- complete%>%
  filter(FIPS %in% cbsa_top30$FIPS)

full<- left_join(cbsa_top30,complete, by="FIPS")

full<-full%>%
  group_by(cbsa, cbsa_name, age_group, PER_TYP)%>%
  summarise(count=n())