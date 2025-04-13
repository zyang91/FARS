setwd("C:/Users/zyang/OneDrive/Desktop/FARS/zy/data/agg/updated")

library(tidyverse)
library(tidycensus)

agg2017<-read_csv("cbsa_agg_rate2017.csv")
agg2018<-read_csv("cbsa_agg_rate2018.csv")
agg2019<-read_csv("cbsa_agg_rate2019.csv")
agg2021<-read_csv("cbsa_agg_rate2021.csv")
agg2022<-read_csv("cbsa_agg_rate2022.csv")

agg2017<-agg2017%>%rename(cbsa_code=`CBSA Code`, cbsa_name=`CBSA Title`)
agg2018<-agg2018%>%rename(cbsa_code=`CBSA Code`, cbsa_name=`CBSA Title`)
agg2019<-agg2019%>%rename(cbsa_code=`CBSA Code`, cbsa_name=`CBSA Title`)
agg2021<-agg2021%>%rename(cbsa_code=`CBSA Code`, cbsa_name=`CBSA Title`)
agg2022<-agg2022%>%rename(cbsa_code=`CBSA Code`, cbsa_name=`CBSA Title`)



## function
aggregation <- function(data) {
  data<-data%>%
    filter(AGE_CATEGORY<5)
  # Add population under 19
  data <- data %>%
    mutate(pop_under18 = AGE0_4 + AGE5_9 + AGE10_14 + AGE15_17)

  # Summarize fatality count by cbsa_code and cbsa_name
  final_summary <- data %>%
    select(cbsa_code, cbsa_name, pop_under18, fatality_count) %>%
    group_by(cbsa_code, cbsa_name) %>%
    summarise(fatality_count = sum(fatality_count, na.rm = TRUE), .groups = "drop")

  # Simplify dataset for pop_under19
  simple_data <- data %>%
    select(cbsa_code, pop_under18)

  # Join summarized data with simplified data
  final <- inner_join(final_summary, simple_data, by = "cbsa_code") %>%
    distinct(cbsa_code, .keep_all = TRUE)

  return(final)
}
final2017<-aggregation(agg2017)
final2018<-aggregation(agg2018)
final2019<-aggregation(agg2019)
final2021<-aggregation(agg2021)
final2022<-aggregation(agg2022)

final2017<-final2017%>%
  mutate(rate=fatality_count/pop_under18*100000)
final2018<-final2018%>%
  mutate(rate=fatality_count/pop_under18*100000)
final2019<-final2019%>%
  mutate(rate=fatality_count/pop_under18*100000)
final2021<-final2021%>%
  mutate(rate=fatality_count/pop_under18*100000)
final2022<-final2022%>%
  mutate(rate=fatality_count/pop_under18*100000)

library(readxl)
cbsa_list<-read_excel("C:/Users/zyang/OneDrive/Desktop/FARS/zy/data/cbsa_list.xlsx")

cbsa_list<-cbsa_list%>%
  rename(cbsa_code=`CBSA Code`)

cbsa_list<-cbsa_list%>%
  select(cbsa_code,`Metropolitan/Micropolitan Statistical Area`)%>%
  filter(`Metropolitan/Micropolitan Statistical Area`=="Metropolitan Statistical Area")%>%
  mutate(cbsa_code=as.numeric(cbsa_code))

#repeat and filter out micrometropoltian statistical area
final2017_joined<-left_join(final2017,cbsa_list, by="cbsa_code")

final2017_joined<-final2017_joined%>%
  distinct(cbsa_code, .keep_all = TRUE)%>%
  filter(!is.na(`Metropolitan/Micropolitan Statistical Area`))

final2018_joined<-left_join(final2018,cbsa_list, by="cbsa_code")

final2018_joined<-final2018_joined%>%
  distinct(cbsa_code, .keep_all = TRUE)%>%
  filter(!is.na(`Metropolitan/Micropolitan Statistical Area`))

final2019_joined<-left_join(final2019,cbsa_list, by="cbsa_code")

final2019_joined<-final2019_joined%>%
  distinct(cbsa_code, .keep_all = TRUE)%>%
  filter(!is.na(`Metropolitan/Micropolitan Statistical Area`))

final2021_joined<-left_join(final2021,cbsa_list, by="cbsa_code")

final2021_joined<-final2021_joined%>%
  distinct(cbsa_code, .keep_all = TRUE)%>%
  filter(!is.na(`Metropolitan/Micropolitan Statistical Area`))

final2022_joined<-left_join(final2022,cbsa_list, by="cbsa_code")

final2022_joined<-final2022_joined%>%
  distinct(cbsa_code, .keep_all = TRUE)%>%
  filter(!is.na(`Metropolitan/Micropolitan Statistical Area`))


final<- rbind(final2017_joined,final2018_joined,final2019_joined,final2021_joined,final2022_joined)

final_agg<-final %>%
  group_by(cbsa_code, cbsa_name)%>%
  summarize(fatality_count=sum(fatality_count), pop_under18=sum(pop_under18))%>%
  mutate(rate=fatality_count/pop_under18*100000)

write_csv(final2017_joined,"cbsa_agg_rate2017.csv")
write_csv(final2018_joined,"cbsa_agg_rate2018.csv")
write_csv(final2019_joined,"cbsa_agg_rate2019.csv")
write_csv(final2021_joined,"cbsa_agg_rate2021.csv")
write_csv(final2022_joined,"cbsa_agg_rate2022.csv")

write_csv(final_agg,"cbsa_agg_rate5yr.csv")
