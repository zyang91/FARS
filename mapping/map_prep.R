library(sf)

setwd("C:/Users/zyang/OneDrive/Desktop/FARS/mapping")
cbsa_shp<- st_read("cbsa_shp/tl_2020_us_cbsa.shp")

str(cbsa_shp)

str(cbsa_group_rate2018)


cbsa_join2018<- left_join(cbsa_shp, cbsa_group_rate2018, by = c("GEOID" = "CBSA Code"))

cbsa_exclude<- cbsa_join2018%>%
  filter(is.na(rate))

st_write(cbsa_exclude, "cbsa_exclude.gpkg")
