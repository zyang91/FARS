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
state_fips$fips <- ifelse(nchar(state_fips$fips ) == 4, paste0("0", state_fips$fips ), state_fips$fips )
state_fips$fips  <- substr(state_fips$fips , 3, nchar(state_fips$fips ))
years= c(2010:2021)
year= 2010 #set the year you want to use

# Functions for parsing 
detailed <- list() #create empty list 
for (x in state_fips$state){ #loop across all states
  counter=0
  for (i in state_fips$fips){ #for the particular state, loop across all county names
    
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
full <- bind_rows(detailed, .id = "column_label")
full <- full %>%  select(where(is.list)) %>% 
  names() %>% 
  reduce(~ unnest_longer(.x, all_of(.y)), .init = full)

get_fars_zip(
  year = 2020,
  format = "csv",
  path = NULL,
  pr = FALSE,
  aux = FALSE,
  read = TRUE,
  geometry = FALSE,
  overwrite = FALSE
)
write.csv(full, 'year.csv')



