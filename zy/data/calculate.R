setwd("C:/Users/zyang/OneDrive/Desktop/data")
library(tidyverse)
library(tidycensus)

#data prep
fata<-read_csv("mastersheetFARS.csv")
pop<-read_csv("allpopdata.csv")

pop<-pop%>%
  select(GEOID,year,AGE0_4,AGE5_9,AGE10_14,AGE15_17,AGE18_19)
 
fata<-fata%>%
  rename(GEOID= county_code, year=YEAR)

full<- full_join(fata,pop, by=c("GEOID","year"))

fata_al<- fata%>%
  filter(STATE==1)%>%
  group_by(COUNTY)%>%
  summarise(total_fatalities=sum(fatality_count))

write.csv(full, "full.csv")
