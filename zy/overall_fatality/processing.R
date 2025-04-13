library(readxl)
library(tidycensus)
library(tidyverse)
library(lubridate)
library(sf)
library(ggrepel)

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

full<-read.csv("cbsa_fatality.csv")

pop2017<-get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = "B01003_001",
  year= 2017,
  survey = "acs1",
  output="wide"
)
pop2018<-get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = "B01003_001",
  year= 2018,
  survey = "acs1",
  output="wide"
)
pop2019<-get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = "B01003_001",
  year= 2019,
  survey = "acs1",
  output="wide"
)
pop2021<-get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = "B01003_001",
  year= 2021,
  survey = "acs1",
  output="wide"
)
pop2022<-get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = "B01003_001",
  year= 2022,
  survey = "acs1",
  output="wide"
)

pop<-rbind(pop2017, pop2018, pop2019, pop2021, pop2022)

pop<-pop%>%
  rename(pop="B01003_001E")%>%
  select(NAME,pop,GEOID)

pop_summary<-pop%>%
  group_by(GEOID)%>%
  summarise(total_pop=sum(pop))

pop_summary$GEOID<-as.numeric(pop_summary$GEOID)
final<-left_join(full, pop_summary, by=c("cbsa_code"="GEOID"))

final<-final%>%
  mutate(fatality_rate=total_fatalities/total_pop*100000)

write.csv(final, "cbsa_fatality_rate.csv", row.names = FALSE)

ggplot(final,aes(x=fatality_rate,y=reorder(title,fatality_rate),fill=total_pop))+
  geom_col()+
  scale_fill_gradient(low="#56B1F7", high="#132B43", name="Population")+
  labs(
    title="CBSA fatality rate",
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

highest30<-highest_50%>%
  arrange(desc(pop_under18))%>%
  slice(1:30)

fatality<-read.csv("cbsa_fatality_rate.csv")
highest30<-highest30%>%
  rename(title=cbsa_name,fatality_rate=rate)

highest30<-highest30%>%
  select(cbsa_code,title,fatality_rate)%>%
  mutate(variable="children")
fatality<-fatality%>%
  select(cbsa_code,title,fatality_rate)%>%
  mutate(variable="total")

full_children_total <- rbind(highest30, fatality)

# not an ideal plot
ggplot(full_children_total,aes(x=fatality_rate,y=reorder(title,fatality_rate),fill=variable))+
  geom_col()+
  facet_wrap(~ variable) +
  scale_fill_manual(values=c("children"="#56B1F7","total"="#132B43"), name="Population")+
  labs(
    title="CBSA fatality rate",
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

full<-left_join(highest30, fatality, by="cbsa_code")
full<-full%>%
  select(cbsa_code,title.x,fatality_rate.x,fatality_rate.y)%>%
  rename(title=title.x,children=fatality_rate.x,total=fatality_rate.y)

write.csv(full, "together.csv")

together<-read.csv("together.csv")


ggplot(together, aes(x=children, y=total,label=title))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text_repel(size = 3, box.padding = 0.5, point.padding = 0.3)+
  labs(
    title="CBSA fatality rate",
    subtitle = "Top 30 most populated, 2017 - 2022",
    x="Children Fatality Rate (per 100,000)",
    y="Total Fatality Rate (per 100,000)"
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
model1<-lm(total~children, data=full)
summary(model1)

# better plot
ggplot(full_children_total, aes(x = fatality_rate,
                  y = reorder(title, fatality_rate),
                  fill = variable,
                  group = variable)) +
  geom_col(aes(color = variable), position = position_dodge(width = 0.8)) +
  scale_color_manual(name = "Group",
                     values = c("Group1" = "#000000", "Group2" = "#FF5733")) +  # adjust groups & colors as needed
  labs(
    title = "CBSA fatality rate",
    subtitle = "Top 30 most populated, 2017 - 2022",
    x = "Fatality Rate (per 100,000)",
    y = "Metropolitan Area"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle    = element_text(hjust = 0.5),
    axis.text.y      = element_text(size = 8),
    axis.title.x     = element_text(size = 12),
    axis.title.y     = element_text(size = 12),
    legend.title     = element_text(size = 10),
    legend.text      = element_text(size = 8),
    legend.key.size  = unit(0.6, "lines")
  )

