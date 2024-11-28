
library(duckdb)
library(tidyverse)

setwd("/Users/apple/Desktop/FARS/zy/Tranditional_data")


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
      WHEN age >= 15 AND age < 18 THEN 4
      WHEN age >= 18 AND age < 20 THEN 5
      WHEN age >= 20 AND age < 200 THEN 6
      WHEN age >= 200 THEN 7
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

write.csv(result, "mastersheetFARS.csv")
#close connection
dbDisconnect(con, shutdown = TRUE)
