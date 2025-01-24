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

#fata_al<- fata%>%
#  filter(STATE==1)%>%
#  group_by(COUNTY)%>%
#  summarise(total_fatalities=sum(fatality_count))

write.csv(full, "full.csv")


# read the data 
full<-read_csv("full.csv")

full<-full%>%
  select(GEOID, year, AGE_CATEGORY, fatality_count, AGE0_4, AGE5_9, AGE10_14, AGE15_17, AGE18_19)

# calculate the percentage of fatalities in each age group
calculated_rate<- full%>%
  mutate(rate= ifelse(AGE_CATEGORY=="1", fatality_count/AGE0_4*1000,
                      ifelse(AGE_CATEGORY=="2", fatality_count/AGE5_9*1000,
                             ifelse(AGE_CATEGORY=="3", fatality_count/AGE10_14*1000,
                                    ifelse(AGE_CATEGORY=="4", fatality_count/AGE15_17*1000,
                                           ifelse(AGE_CATEGORY=="5", fatality_count/AGE18_19*1000, NA))))))

#test_with age group 1
# calculated_rate_age1<- calculated_rate%>%
#   filter(AGE_CATEGORY=="1")%>%
#   select(GEOID, year, rate)

#simple version datasets extract
rate<-calculated_rate%>%
  select(GEOID, year, AGE_CATEGORY, fatality_count, rate)

full<-calculated_rate%>%
  filter(!is.na(rate))%>%
  filter(year>2016)%>%
  filter(year!=2020)

rate<-rate%>%
  filter(!is.na(rate))%>%
  filter(year>2016)%>%
  filter(year!=2020)


# export the data
write.csv(full, "full_filtered.csv")
write.csv(rate,"rate_simple_filtered.csv")




