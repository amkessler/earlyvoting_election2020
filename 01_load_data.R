library(tidyverse)
library(janitor)
library(lubridate)

#import raw daily files for 2020
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



#import raw historical files for 2016
state_2016 <- read_tsv("raw_data/2016_avev_delivery_state.txt.gz", col_types = cols(.default = "c"))
county_2016 <- read_tsv("raw_data/2016_avev_delivery_county.txt.gz", col_types = cols(.default = "c"))


