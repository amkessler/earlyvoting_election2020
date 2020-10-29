library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(writexl)
library(tidycensus)
options(scipen = 999)

#set the variable for days before the election
### CHANGE THIS DAILY AS NEEDED
before_election_choice <- 6


# load processed data files from step 01
state_latest <- readRDS("processed_data/state_latest.rds")
county_latest <- readRDS("processed_data/county_latest.rds")
state_2016 <- readRDS("processed_data/state_2016.rds")
county_2016 <- readRDS("processed_data/county_2016.rds")

#grand total sum from state_latest
state_latest %>% 
  summarise(sum(ballots_returned))

state_latest %>% 
  filter(state == "PA") %>% 
  summarise(sum(ballots_returned))


#filter all the datasets based on days before election desired
state_latest <- state_latest %>% 
  filter(days_to_election >= before_election_choice)

county_latest <- county_latest %>% 
  filter(days_to_election >= before_election_choice)

state_2016 <- state_2016 %>% 
  filter(days_to_election >= before_election_choice)

county_2016 <- county_2016 %>% 
  filter(days_to_election >= before_election_choice)


#add new column to use for cycle identification, then combine records
state_latest <- state_latest %>% 
  mutate(
    cycle = "2020"
  ) %>% 
  select(cycle, everything())

state_2016 <- state_2016 %>% 
  mutate(
    cycle = "2016"
  ) %>% 
  select(cycle, everything())

#combine
state_bothcycles <- bind_rows(state_latest, state_2016)
state_bothcycles

saveRDS(state_bothcycles, "processed_data/state_bothcycles.rds")


#now for counties
county_latest <- county_latest %>% 
  mutate(
    cycle = "2020"
  ) %>% 
  select(cycle, everything())

county_2016 <- county_2016 %>% 
  mutate(
    cycle = "2016"
  ) %>% 
  select(cycle, everything())

#combine
county_bothcycles <- bind_rows(county_latest, county_2016)
county_bothcycles

saveRDS(county_bothcycles, "processed_data/county_bothcycles.rds")



### SUMMARY TOP LEVEL BREAKDOWNS ##### ---------------------------------------------


### NATIONAL ####

### calculate grand totals nationally for each cycle
#we'll just to returned here, since some states deal with requested differently in 2020
natl_grandtots_bothyears <- state_bothcycles %>% 
  group_by(cycle) %>% 
  summarise(
    total_returned = sum(ballots_returned, na.rm = TRUE)
  ) 

natl_grandtots_bothyears

#reshape to wide format to allow for comparison calculations
natl_grandtots_bothyears <- natl_grandtots_bothyears %>% 
  pivot_wider(names_from = cycle, values_from = total_returned) %>% 
  clean_names()

#calculate change
natl_grandtots_bothyears <- natl_grandtots_bothyears %>% 
  mutate(
    diff_returned = x2020 - x2016,
    pctchg_returned = round_half_up((x2020 - x2016) / x2016 * 100, 2)
  ) 

natl_grandtots_bothyears

#save output to file
filename_natltots <- paste0("output/natl_grandtots_bothyears_", before_election_choice, "daysout.xlsx")
write_xlsx(natl_grandtots_bothyears, filename_natltots)

#save rds version for use with subsequent markdown file if needed to avoid running entire script
saveRDS(natl_grandtots_bothyears, "processed_data/natl_grandtots_bothyears.rds")



### STATE ####

state_bothcycles %>% 
  filter(state == "PA") %>% 
  group_by(cycle) %>% 
  summarise(sum(ballots_returned, na.rm = TRUE))


#calculate state grand totals for early voting
state_grandtots_bothyears <- state_bothcycles %>% 
  group_by(cycle, state) %>% 
  summarise(
    total_requested = sum(ballots_requested, na.rm = TRUE),
    total_returned = sum(ballots_returned, na.rm = TRUE)
  ) %>% 
  pivot_wider(names_from = cycle, values_from = c(total_requested, total_returned))

#calculate change
state_grandtots_bothyears <- state_grandtots_bothyears %>% 
  mutate(
    diff_requested = total_requested_2020 - total_requested_2016,
    pctchg_requested = round_half_up((total_requested_2020 - total_requested_2016) / total_requested_2016 * 100, 2),
    diff_returned = total_returned_2020 - total_returned_2016,
    pctchg_returned = round_half_up((total_returned_2020 - total_returned_2016) / total_returned_2016 * 100, 2)
  ) 

state_grandtots_bothyears

#deal with how several states don't report requested, only returned in 2020
#the data structure deals with this by putting the identicial returned number into requested 
#we'll change such instances to NA for the change calculations to avoid confusion
state_grandtots_bothyears <- state_grandtots_bothyears %>% 
  mutate(
    diff_requested = na_if(total_requested_2020, total_returned_2020),
    pctchg_requested = na_if(total_requested_2020, total_returned_2020),
    total_requested_2020 = na_if(total_requested_2020, total_returned_2020)
    )

#save output to file
filename_statetots <- paste0("output/state_grandtots_bothyears_", before_election_choice, "daysout.xlsx")
write_xlsx(state_grandtots_bothyears, filename_statetots)

#save rds version for use with subsequent markdown file if needed to avoid running entire script
saveRDS(state_grandtots_bothyears, "processed_data/state_grandtots_bothyears.rds")




### COUNTY ####

# calculate county grand totals for early voting
county_grandtots_bothyears <- county_bothcycles %>% 
  group_by(cycle, countyfips) %>% 
  summarise(
    total_requested = sum(ballots_requested, na.rm = TRUE),
    total_returned = sum(ballots_returned, na.rm = TRUE)
  ) %>% 
  pivot_wider(names_from = cycle, values_from = c(total_requested, total_returned))

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

#join to main table
county_grandtots_bothyears <- inner_join(fips_lookuptable, county_grandtots_bothyears, by = "countyfips")

county_grandtots_bothyears

#deal with how several states don't report requested, only returned in 2020
#the data source deals with this by putting the identicial returned number into requested 
#we'll change such instances to NA 
county_grandtots_bothyears <- county_grandtots_bothyears %>% 
  mutate(
    diff_requested = na_if(total_requested_2020, total_returned_2020),
    pctchg_requested = na_if(total_requested_2020, total_returned_2020),
    total_requested_2020 = na_if(total_requested_2020, total_returned_2020)
  )

#save output to file
filename_countytots <- paste0("output/county_grandtots_bothyears_", before_election_choice, "daysout.xlsx")
write_xlsx(county_grandtots_bothyears, filename_countytots)

#save rds version for use with subsequent markdown file if needed to avoid running entire script
saveRDS(county_grandtots_bothyears, "processed_data/county_grandtots_bothyears.rds")



