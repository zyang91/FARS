setwd("C:/Users/zyang/OneDrive/Desktop/FARS/zy/data")
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




full<-full%>%
  select(GEOID, year, AGE_CATEGORY, fatality_count, AGE0_4, AGE5_9, AGE10_14, AGE15_17, AGE18_19)

# calculate the percentage of fatalities in each age group
full<- full%>%
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

full<-full%>%
  filter(!is.na(rate))%>%
  filter(year>2016)%>%
  filter(year!=2020)

fars_2017<-full%>%
  filter(year==2017)

library(readxl)
cbsa<-read_excel("cbsa_list.xlsx")

cbsa<- cbsa%>%
  mutate(GEOID=sprintf("%02d%03d", as.numeric(cbsa$`FIPS State Code`), as.numeric(cbsa$`FIPS County Code`)))

cbsa<-cbsa%>%
  select(`CBSA Code`, GEOID,`CBSA Title`,`Metropolitan/Micropolitan Statistical Area`,`CSA Title`,`County/County Equivalent`)

full_joined<-full_join(fars_2017,cbsa, by="GEOID")

full_join_filter<-full_joined%>%
  filter(!is.na(year))

cbsa_group<-full_join_filter%>%
  group_by(`CBSA Code`,`CBSA Title`, AGE_CATEGORY)%>%
  summarise(fatality_count=sum(fatality_count),
            AGE0_4=sum(AGE0_4),
            AGE5_9=sum(AGE5_9),
            AGE10_14=sum(AGE10_14),
            AGE15_17=sum(AGE15_17),
            AGE18_19=sum(AGE18_19))

cbsa_group_rate<- cbsa_group%>%
  mutate(rate= ifelse(AGE_CATEGORY=="1", fatality_count/AGE0_4*1000,
                      ifelse(AGE_CATEGORY=="2", fatality_count/AGE5_9*1000,
                             ifelse(AGE_CATEGORY=="3", fatality_count/AGE10_14*1000,
                                    ifelse(AGE_CATEGORY=="4", fatality_count/AGE15_17*1000,
                                           ifelse(AGE_CATEGORY=="5", fatality_count/AGE18_19*1000, NA))))))

write_csv(cbsa_group_rate, "cbsa_agg_rate2017.csv")





#2018
fars_2018<-full%>%
  filter(year==2018)

full_joined2018<-full_join(fars_2018,cbsa, by="GEOID")

full_join_filter2018<-full_joined2018%>%
  filter(!is.na(year))

cbsa_group2018<-full_join_filter2018%>%
  group_by(`CBSA Code`,`CBSA Title`, AGE_CATEGORY)%>%
  summarise(fatality_count=sum(fatality_count),
            AGE0_4=sum(AGE0_4),
            AGE5_9=sum(AGE5_9),
            AGE10_14=sum(AGE10_14),
            AGE15_17=sum(AGE15_17),
            AGE18_19=sum(AGE18_19))

cbsa_group_rate2018<- cbsa_group2018%>%
  mutate(rate= ifelse(AGE_CATEGORY=="1", fatality_count/AGE0_4*1000,
                      ifelse(AGE_CATEGORY=="2", fatality_count/AGE5_9*1000,
                             ifelse(AGE_CATEGORY=="3", fatality_count/AGE10_14*1000,
                                    ifelse(AGE_CATEGORY=="4", fatality_count/AGE15_17*1000,
                                           ifelse(AGE_CATEGORY=="5", fatality_count/AGE18_19*1000, NA))))))

write_csv(cbsa_group_rate2018, "cbsa_agg_rate2018.csv")

#2019
fars_2019<-full%>%
  filter(year==2019)

full_joined2019<-full_join(fars_2019,cbsa, by="GEOID")

full_join_filter2019<-full_joined2019%>%
  filter(!is.na(year))

cbsa_group2019<-full_join_filter2019%>%
  group_by(`CBSA Code`,`CBSA Title`, AGE_CATEGORY)%>%
  summarise(fatality_count=sum(fatality_count),
            AGE0_4=sum(AGE0_4),
            AGE5_9=sum(AGE5_9),
            AGE10_14=sum(AGE10_14),
            AGE15_17=sum(AGE15_17),
            AGE18_19=sum(AGE18_19))

cbsa_group_rate2019<- cbsa_group2019%>%
  mutate(rate= ifelse(AGE_CATEGORY=="1", fatality_count/AGE0_4*1000,
                      ifelse(AGE_CATEGORY=="2", fatality_count/AGE5_9*1000,
                             ifelse(AGE_CATEGORY=="3", fatality_count/AGE10_14*1000,
                                    ifelse(AGE_CATEGORY=="4", fatality_count/AGE15_17*1000,
                                           ifelse(AGE_CATEGORY=="5", fatality_count/AGE18_19*1000, NA))))))

write_csv(cbsa_group_rate2019, "cbsa_agg_rate2019.csv")

#2021
fars_2021<-full%>%
  filter(year==2021)

full_joined2021<-full_join(fars_2021,cbsa, by="GEOID")

full_join_filter2021<-full_joined2021%>%
  filter(!is.na(year))

cbsa_group2021<-full_join_filter2021%>%
  group_by(`CBSA Code`,`CBSA Title`, AGE_CATEGORY)%>%
  summarise(fatality_count=sum(fatality_count),
            AGE0_4=sum(AGE0_4),
            AGE5_9=sum(AGE5_9),
            AGE10_14=sum(AGE10_14),
            AGE15_17=sum(AGE15_17),
            AGE18_19=sum(AGE18_19))

cbsa_group_rate2021<- cbsa_group2021%>%
  mutate(rate= ifelse(AGE_CATEGORY=="1", fatality_count/AGE0_4*1000,
                      ifelse(AGE_CATEGORY=="2", fatality_count/AGE5_9*1000,
                             ifelse(AGE_CATEGORY=="3", fatality_count/AGE10_14*1000,
                                    ifelse(AGE_CATEGORY=="4", fatality_count/AGE15_17*1000,
                                           ifelse(AGE_CATEGORY=="5", fatality_count/AGE18_19*1000, NA))))))

write_csv(cbsa_group_rate2021, "cbsa_agg_rate2021.csv")

#2022
fars_2022<-full%>%
  filter(year==2022)

full_joined2022<-full_join(fars_2022,cbsa, by="GEOID")

full_join_filter2022<-full_joined2022%>%
  filter(!is.na(year))

cbsa_group2022<-full_join_filter2022%>%
  group_by(`CBSA Code`,`CBSA Title`, AGE_CATEGORY)%>%
  summarise(fatality_count=sum(fatality_count),
            AGE0_4=sum(AGE0_4),
            AGE5_9=sum(AGE5_9),
            AGE10_14=sum(AGE10_14),
            AGE15_17=sum(AGE15_17),
            AGE18_19=sum(AGE18_19))

cbsa_group_rate2022<- cbsa_group2022%>%
  mutate(rate= ifelse(AGE_CATEGORY=="1", fatality_count/AGE0_4*1000,
                      ifelse(AGE_CATEGORY=="2", fatality_count/AGE5_9*1000,
                             ifelse(AGE_CATEGORY=="3", fatality_count/AGE10_14*1000,
                                    ifelse(AGE_CATEGORY=="4", fatality_count/AGE15_17*1000,
                                           ifelse(AGE_CATEGORY=="5", fatality_count/AGE18_19*1000, NA))))))

write_csv(cbsa_group_rate2022, "cbsa_agg_rate2022.csv")
