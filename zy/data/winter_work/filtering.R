setwd("C:/Users/zyang/Downloads")

library(tidyverse)

full<-read.csv("full.csv")

full<-full%>%
  filter(!is.na(rate))%>%
  filter(year>2016)%>%
  filter(year!=2020)