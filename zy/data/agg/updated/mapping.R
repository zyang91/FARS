setwd("/Users/zhanchaoyang/Desktop/FARS/zy/data/agg/updated")

library(tidyverse)
library(lubridate)
library(sf)

# Load the data
fars5yrs<- read_csv("cbsa_agg_rate5yr.csv")

# filter the data
highest_rate <- fars5yrs %>%
  arrange(desc(rate)) %>%
  slice_head(n=50)

highest_pop<- fars5yrs %>%
  arrange(desc(pop_under18)) %>%
  slice_head(n=50)

highest_pop30<- fars5yrs %>%
  arrange(desc(pop_under18)) %>%
  slice_head(n=30)

highest_pop30<- highest_pop30 %>%
  arrange(desc(rate))
f2017<- read_csv("cbsa_agg_rate2017.csv")
f2018<- read_csv("cbsa_agg_rate2018.csv")
f2019<- read_csv("cbsa_agg_rate2019.csv")
f2021<- read_csv("cbsa_agg_rate2021.csv")
f2022<- read_csv("cbsa_agg_rate2022.csv")

f2017<- f2017 %>%
  mutate(year=2017)
f2018<- f2018 %>%
  mutate(year=2018)
f2019<- f2019 %>%
  mutate(year=2019)
f2021<- f2021 %>%
  mutate(year=2021)
f2022<- f2022 %>%
  mutate(year=2022)

full<-rbind(f2017,f2018,f2019,f2021,f2022)

highest10<- fars5yrs %>%
  arrange(desc(pop_under18)) %>%
  slice_head(n=10)

full1<- full %>%
  filter(cbsa_code %in% highest10$cbsa_code)

ggplot(full1, aes(x=year, y=rate, group=cbsa_name, color=cbsa_name))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(title="Top 10 CBSA with Highest Population Under 18", x="Year", y="Rate")+
  theme(legend.position = "right", legend.text = element_text(size=5), title = element_text(size=15))

highest10_rate<- fars5yrs %>%
  arrange(desc(rate)) %>%
  slice_head(n=10)

full2<- full %>%
  filter(cbsa_code %in% highest10_rate$cbsa_code)

ggplot(full2, aes(x=year, y=rate, group=cbsa_name, color=cbsa_name))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(title="Top 10 CBSA with Highest Rate", x="Year", y="Rate")+
  theme(legend.position = "right", legend.text = element_text(size=5), title = element_text(size=15))


write.csv(highest_rate, "highest_rate50.csv")
write.csv(highest_pop, "highest_pop50.csv")

ggplot(highest_pop30,aes(x=rate,y=reorder(cbsa_name,rate),fill=pop_under18))+
  geom_col()+
  scale_fill_gradient(low="#56B1F7", high="#132B43", name="Population")+
  labs(
    title="CBSA with Highest Population Under 18",
    subtitle = "Top 30 most populated, 2017 - 2022",
    x="Fatality Rate (per 100,000)",
    y="Metropolitan Area"
  )+
  theme_minimal(base_size=12)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 10),  # Smaller legend title
    legend.text = element_text(size = 8),   # Smaller legend text
    legend.key.size = unit(0.6, "lines")
  )
