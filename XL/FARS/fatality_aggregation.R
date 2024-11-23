library(duckdb)
library(tidyverse)
library(ggplot2)
library(sf)
library(mapproj)

mapTheme <- theme(plot.title =element_text(size=12),
                  plot.subtitle = element_text(size=8),
                  plot.caption = element_text(size = 6),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(colour = 'transparent'),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "right",
                  
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))
q5 <- function(variable) {as.factor(ntile(variable, 5))}
flatreds5 <- c('#f9ebea','#e6b0aa','#c2665b', '#a33428','#7b241c')
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],3),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]],
                                  c(.01,.2,.4,.6,.8), na.rm=T),
                         digits = 3))
  }
}



current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
con <- dbConnect(duckdb::duckdb(), dbdir = "raw_data.duckdb")

dbListTables(con)


query <- "
  SELECT 
    YEAR,
    STATE,
    COUNTY,
    CASE 
      WHEN age < 5 THEN 1
      WHEN age >= 5 AND age < 10 THEN 2
      WHEN age >= 10 AND age < 15 THEN 3
      WHEN age >= 15 AND age < 19 THEN 4
      WHEN age >= 19 AND age < 200 THEN 5
      WHEN age >= 200 THEN 6
      ELSE NULL
    END AS AGE_CATEGORY,
    COUNT(*) AS fatality_count
  FROM (
    SELECT '2010' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2010
    UNION ALL
    SELECT '2011' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2011
    UNION ALL
    SELECT '2012' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2012
    UNION ALL
    SELECT '2013' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2013
    UNION ALL
    SELECT '2014' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2014
    UNION ALL
    SELECT '2015' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2015
    UNION ALL
    SELECT '2016' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2016
    UNION ALL
    SELECT '2017' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2017
    UNION ALL
    SELECT '2018' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2018
    UNION ALL
    SELECT '2019' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2019
    UNION ALL
    SELECT '2020' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2020
    UNION ALL
    SELECT '2021' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2021
    UNION ALL
    SELECT '2022' AS YEAR, state AS STATE, county AS COUNTY, age, INJ_SEV FROM person2022
  ) AS all_YEARs
  WHERE INJ_SEV = 4
  GROUP BY YEAR, STATE, COUNTY, AGE_CATEGORY
  ORDER BY YEAR, STATE, COUNTY, AGE_CATEGORY
"


# Execute the query
result <- dbGetQuery(con, query)

#create unique fips code
result<- result%>%
  mutate(county_code= sprintf("%02d%03d", result$STATE, result$COUNTY) )

write.csv(result, "county_fat_w_age.csv")
#close connection
dbDisconnect(con, shutdown = TRUE)
state_dets <- read.csv('./state-details.csv')
state_dets$stusps <- substr(state_dets$stusps, 2, nchar(state_dets$stusps))
states_sf <- st_read('./us_states_hexgrid.gpkg') %>% left_join(state_dets, by=c('iso3166_2'='stusps')) %>% 
  left_join(result, by=c('st'='STATE')) %>% st_transform(3857)#project to mercator pcs
text_inv <- c('black', 'white','white','white','white')
ggplot(states_sf) +
  geom_sf(aes(fill= q5(entry_count)), color='white') +
  scale_fill_manual(values = flatreds5,
                     labels = qBr(states_sf, 'entry_count'), 
                     name = 'Number of Fatalities') +
  geom_sf_text(aes(label = iso3166_2, color = q5(entry_count)), size=2 )+
  scale_color_manual(values=text_inv)+
  mapTheme+
  guides(color = "none")+
  
  theme(legend.position = 'right')+
  labs(title='Change in Child Traffic Fatalities across US states',
       subtitle = 'Aged 19 and under'
       )+
  
  facet_wrap('YEAR',ncol=4)





