library(tidyverse)
library(janitor)
library(lubridate)
library(writexl)
library(tidycensus)
options(scipen = 999)

# load processed data files from step 01
state_latest <- readRDS("processed_data/state_latest.rds")
county_latest <- readRDS("processed_data/county_latest.rds")
state_2016 <- readRDS("processed_data/state_2016.rds")
county_2016 <- readRDS("processed_data/county_2016.rds")


#set the variable for days before the election
### CHANGE THIS DAILY AS NEEDED
before_election_choice <- 13


#filter all the datasets based on days before election desired
state_latest <- state_latest %>% 
  filter(days_to_election >= before_election_choice)

county_latest <- county_latest %>% 
  filter(days_to_election >= before_election_choice)

state_2016 <- state_2016 %>% 
  filter(days_to_election >= before_election_choice)

county_2016 <- county_2016 %>% 
  filter(days_to_election >= before_election_choice)



##### GRAND TOTAL COUNTS AND COMPARISONS ####-------------------------------------------------

### NATIONAL ####

#calculate a national total for early voting
#2020
natl_grandtots_2020 <- state_latest %>% 
  summarise(total_requested_2020 = sum(ballots_requested, na.rm = TRUE),
            total_returned_2020 = sum(ballots_returned, na.rm = TRUE)
            )

natl_grandtots_2020

#2016
natl_grandtots_2016 <- state_2016 %>% 
  summarise(total_requested_2016 = sum(ballots_requested, na.rm = TRUE),
            total_returned_2016 = sum(ballots_returned, na.rm = TRUE)
  )

natl_grandtots_2016

#combine years into one table
natl_grandtots_bothyears <- bind_cols(natl_grandtots_2016, natl_grandtots_2020)

#calculate change
natl_grandtots_bothyears <- natl_grandtots_bothyears %>% 
  mutate(
    diff_requested = total_requested_2020 - total_requested_2016,
    pctchg_requested = round_half_up((total_requested_2020 - total_requested_2016) / total_requested_2016 * 100, 2),
    diff_returned = total_returned_2020 - total_returned_2016,
    pctchg_returned = round_half_up((total_returned_2020 - total_returned_2016) / total_returned_2016 * 100, 2)
  ) 

natl_grandtots_bothyears

#save output to file
write_xlsx(natl_grandtots_bothyears, "output/natl_grandtots_bothyears.xlsx")



### STATE LEVEL ####

state_latest %>%
  group_by(state) %>%
  summarise(total_requested_2020 = sum(ballots_requested, na.rm = TRUE)
  )

#calculate state grand totals for early voting
#2020
state_grandtots_2020 <- state_latest %>% 
  group_by(state) %>%
  summarise(total_requested_2020 = sum(ballots_requested, na.rm = TRUE),
            total_returned_2020 = sum(ballots_returned, na.rm = TRUE)
  )

state_grandtots_2020

#2016
state_grandtots_2016 <- state_2016 %>% 
  group_by(state) %>%
  summarise(total_requested_2016 = sum(ballots_requested, na.rm = TRUE),
            total_returned_2016 = sum(ballots_returned, na.rm = TRUE)
  )

state_grandtots_2016

#join years into one table
state_grandtots_bothyears <- left_join(state_grandtots_2020, state_grandtots_2016, by = "state") %>% 
                                    select(state, total_requested_2016, total_returned_2016, everything())

state_grandtots_bothyears


#calculate change
state_grandtots_bothyears <- state_grandtots_bothyears %>% 
  mutate(
    diff_requested = total_requested_2020 - total_requested_2016,
    pctchg_requested = round_half_up((total_requested_2020 - total_requested_2016) / total_requested_2016 * 100, 2),
    diff_returned = total_returned_2020 - total_returned_2016,
    pctchg_returned = round_half_up((total_returned_2020 - total_returned_2016) / total_returned_2016 * 100, 2)
  ) 

state_grandtots_bothyears

#save output to file
write_xlsx(state_grandtots_bothyears, "output/state_grandtots_bothyears.xlsx")


### COUNTY LEVEL ####

county_latest %>%
  group_by(countyfips) %>%
  summarise(total_requested_2020 = sum(ballots_requested, na.rm = TRUE)
  )

#calculate county grand totals for early voting
#2020
county_grandtots_2020 <- county_latest %>% 
  group_by(countyfips) %>%
  summarise(total_requested_2020 = sum(ballots_requested, na.rm = TRUE),
            total_returned_2020 = sum(ballots_returned, na.rm = TRUE)
  )

county_grandtots_2020

#2016
county_grandtots_2016 <- county_2016 %>% 
  group_by(countyfips) %>%
  summarise(total_requested_2016 = sum(ballots_requested, na.rm = TRUE),
            total_returned_2016 = sum(ballots_returned, na.rm = TRUE)
  )

county_grandtots_2016

#join years into one table
county_grandtots_bothyears <- left_join(county_grandtots_2020, county_grandtots_2016, by = "countyfips") %>% 
  select(countyfips, total_requested_2016, total_returned_2016, everything())

county_grandtots_bothyears


#calculate change
county_grandtots_bothyears <- county_grandtots_bothyears %>% 
  mutate(
    diff_requested = total_requested_2020 - total_requested_2016,
    pctchg_requested = round_half_up((total_requested_2020 - total_requested_2016) / total_requested_2016 * 100, 2),
    diff_returned = total_returned_2020 - total_returned_2016,
    pctchg_returned = round_half_up((total_returned_2020 - total_returned_2016) / total_returned_2016 * 100, 2)
  ) 

county_grandtots_bothyears

#bring in fips lookup table from tidycensus package to add names etc to accompany fips codes
head(fips_codes)

fips_lookuptable <- fips_codes %>% 
  as_tibble() %>% 
  mutate(
    countyfips = paste0(state_code, county_code)
  ) %>% 
  select(countyfips, everything(), -state_code, -county_code)


#save output to file
write_xlsx(county_grandtots_bothyears, "output/county_grandtots_bothyears.xlsx")



