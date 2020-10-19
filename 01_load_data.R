library(tidyverse)
library(janitor)
library(lubridate)

#import raw daily files for 2020 ####
state_latest <- read_tsv("raw_data/20201019_1_avev_delivery_state.txt.gz", col_types = cols(.default = "c"))
county_latest <- read_tsv("raw_data/20201019_1_avev_delivery_county.txt.gz", col_types = cols(.default = "c"))

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


### save results for use in analysis steps ####
saveRDS(state_latest, "processed_data/state_latest.rds")
saveRDS(county_latest, "processed_data/county_latest.rds")
saveRDS(state_2016, "processed_data/state_2016.rds")
saveRDS(county_2016, "processed_data/county_2016.rds")

### save csv versions for sharing with others
write_csv(state_latest, "processed_data/state_latest.csv")
write_csv(county_latest, "processed_data/county_latest.csv")
write_csv(state_2016, "processed_data/state_2016.csv")
write_csv(county_2016, "processed_data/county_2016.csv")
