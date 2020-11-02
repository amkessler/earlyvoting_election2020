library(tidyverse)
library(janitor)
library(lubridate)

#import raw daily files for 2020 ####
#when downloading new files, rename the new ones to have "latest" in place of their proceeding datestamp
state_latest <- read_tsv("raw_data/latest_avev_delivery_state.txt.gz", col_types = cols(.default = "c"))
county_latest <- read_tsv("raw_data/latest_avev_delivery_county.txt.gz", col_types = cols(.default = "c"))

#format columns
state_latest

state_latest <- state_latest %>% 
  mutate(
    days_to_election = as.numeric(days_to_election),
    ballots_requested = as.numeric(ballots_requested),
    ballots_returned = as.numeric(ballots_returned)
  )

county_latest <- county_latest %>% 
  mutate(
    days_to_election = as.numeric(days_to_election),
    ballots_requested = as.numeric(ballots_requested),
    ballots_returned = as.numeric(ballots_returned)
  )

#fix fips codes with missing leading zeros
county_latest <- county_latest %>% 
  mutate(
    countyfips = if_else(str_length(countyfips) == 4, paste0("0", countyfips), countyfips)
  ) 

#check results
county_latest %>% 
  filter(state == "AZ")


#import raw historical files for 2016 ####
state_2016 <- read_tsv("raw_data/2016_avev_delivery_state.txt.gz", col_types = cols(.default = "c"))
county_2016 <- read_tsv("raw_data/2016_avev_delivery_county.txt.gz", col_types = cols(.default = "c"))


#format columns
state_2016 <- state_2016 %>% 
  mutate(
    days_to_election = as.numeric(days_to_election),
    ballots_requested = as.numeric(ballots_requested),
    ballots_returned = as.numeric(ballots_returned)
  )

county_2016 <- county_2016 %>% 
  mutate(
    days_to_election = as.numeric(days_to_election),
    ballots_requested = as.numeric(ballots_requested),
    ballots_returned = as.numeric(ballots_returned)
  )

#fix fips codes with missing leading zeros
county_2016 <- county_2016 %>% 
  mutate(
    countyfips = if_else(str_length(countyfips) == 4, paste0("0", countyfips), countyfips)
  ) 

#check results
county_2016 %>% 
  filter(state == "AZ")



### save results for use in subsequent analysis steps ####
saveRDS(state_latest, "processed_data/state_latest.rds")
saveRDS(county_latest, "processed_data/county_latest.rds")
saveRDS(state_2016, "processed_data/state_2016.rds")
saveRDS(county_2016, "processed_data/county_2016.rds")

### save csv versions for sharing with others
write_csv(state_latest, "processed_data/state_latest.csv")
write_csv(county_latest, "processed_data/county_latest.csv")
write_csv(state_2016, "processed_data/state_2016.csv")
write_csv(county_2016, "processed_data/county_2016.csv")


#show most recent day to election included
state_latest %>% 
  count(days_to_election) %>% 
  arrange(days_to_election)

state_latest %>% 
  filter(days_to_election == 2) 

state_latest %>% 
  filter(days_to_election == 2) %>% 
  count(state)
