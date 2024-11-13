library(pak)
pak::pkg_install("elipousson/crashapi")

library(crashapi)
library(tidyverse)

md_summary <-
  get_fars(
    year = c(2010, 2021),
    state = "MD",
    api = "summary count"
  )

ggplot(md_summary, aes(x = CaseYear, y = TotalFatalCounts)) +
  geom_point(color = "red") +
  geom_line(color = "red", group = 1) +
  theme_minimal()

#2022 data avilability test
# Notes:
# 110 variable available

crashes_autauga2022 <-
  get_fars(
    year = 2022,
    state = "AL",
    county = "Autauga County",
    details = TRUE
  )

autauga2022<-crashes_autauga2022 %>%
  select(CITYNAME,
         FATALS)


crashes_wade <-
  get_fars(
    year = 2022,
    state = "NC",
    county = "Wake County",
    details = TRUE
  )

crashes_broome <-
  get_fars(
    year=2022,
    state="NY",
    county="Broome County",
    details=TRUE
  )

