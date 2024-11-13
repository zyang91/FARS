#A script which automatically downloads yearly aggregates of FARS data. The only user input required is the year.
#Xian Lu, Lee

library(crashapi)
library(ggplot2)
library(dplyr)
library(rstudioapi)
library(tidyr)
library(purrr)
# Set Up
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
state_fips<-read.csv('./data/fips-by-state.csv')#get state and counties
years= c(2010:2015)
year= 2010 #set the year you want to use

# Functions for parsing 
detailed <- list() #create empty list 
for (x in state_fips$state){ #loop across all states
  counter=0
  for (i in state_fips$name){ #for the particular state, loop across all county names
    
    crashes_detailed <- #for each county, get the detailed crash data for the year
      get_fars(
        year = year,
        state = x,
        county= i,
        details = TRUE
      )
    counter=counter+1
    detailed[[length(detailed)+1]] <- crashes_detailed
    cat(counter, x, i , "downloaded")#tracking
  }
  
}
# 
# test <- detailed[1]
#unnest cols = c(CEvents, NMDrugs, NPersons, NmCrashes, NmImpairs, NmPriors, SafetyEQs, Vehicles)
full <- bind_rows(detailed, .id = "column_label") %>%  select(where(is.list)) %>% 
  names() %>% 
  reduce(~ unnest_longer(.x, all_of(.y)), .init = full)

write.csv(full, 'year.csv')



