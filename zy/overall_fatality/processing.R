library(readxl)
library(tidycensus)
library(tidyverse)
library(lubridate)
library(sf)

# load the data
list<-read_excel("data/cbsa_list.xlsx")
highest_50<-read.csv("data/highest_pop50.csv")
FARS<-read.csv("data/mastersheetFARS.csv")

highest_30<-highest_50%>%
  arrange(desc(pop_under18))%>%
  slice(1:30)

str(FARS)

FARS$fips<-sprintf("%02d%03d", FARS$STATE, FARS$COUNTY)

FARS<- FARS%>%
  select(-STATE, -COUNTY,-county_code)

FARS<- FARS %>%
  filter(YEAR!=2020)%>%
  filter(YEAR>2016)

list_30<-list%>%
  filter(`CBSA Code`%in%highest_30$cbsa_code)%>%
  rename(cbsa_code="CBSA Code")

list_30<-list_30%>%
  rename(county= "FIPS County Code", state="FIPS State Code")

list_30<-list_30 %>%
  rename(title="CBSA Title")

list30<-list_30%>%
  select(cbsa_code, title, state, county)

list30$state<-as.numeric(list30$state)
list30$county<-as.numeric(list30$county)

list30$countyfips<-sprintf("%02d%03d", list30$state, list30$county)

FARS<-FARS%>%
  filter(fips%in%list30$countyfips)

FARS_summary<-FARS%>%
  group_by(fips)%>%
  summarise(total_fatalities=sum(fatality_count))
full<-left_join(list30, FARS_summary, by=c("countyfips"="fips"))

# check NA
#check<-FARS%>%
#  filter(county_code== 51610)

full<-full%>%
  filter(!is.na(total_fatalities))

full<-full%>%
  group_by(title,cbsa_code)%>%
  summarise(total_fatalities=sum(total_fatalities))

write.csv(full, "cbsa_fatality.csv", row.names = FALSE)
