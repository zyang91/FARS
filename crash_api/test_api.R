library(pak)
pak::pkg_install("elipousson/crashapi")

library(crashapi)
library(ggplot2)

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

crashes_sf <-
  get_fars(
    year = c(2018, 2021),
    state = "NC",
    county = "Wake County",
    geometry = TRUE
  )
