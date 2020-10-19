library(tidyverse)
library(janitor)
library(lubridate)
library(writexl)
options(scipen = 999)

# load processed data files from step 01
state_latest <- readRDS("processed_data/state_latest.rds")
county_latest <- readRDS("processed_data/county_latest.rds")
state_2016 <- readRDS("processed_data/state_2016.rds")
county_2016 <- readRDS("processed_data/county_2016.rds")


#set the variable for days before the election
before_election_choice <- 14

#filter all the datasets based on days before election desired
state_latest <- state_latest %>% 
  filter(days_to_election >= before_election_choice)

county_latest <- county_latest %>% 
  filter(days_to_election >= before_election_choice)

state_2016 <- state_2016 %>% 
  filter(days_to_election >= before_election_choice)

county_2016 <- county_2016 %>% 
  filter(days_to_election >= before_election_choice)



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


# 
# state_latest %>% 
#   group_by(state) %>% 
#   summarise(total_requested_2020 = sum(ballots_requested, na.rm = TRUE)
#   )
# 
# 
# #group into aggregate totals by state
# statetotals <- statecnt %>% 
#   group_by(state) %>% 
#   summarise(total_advance_2018 = sum(ballot_return_count_2018, na.rm = TRUE),
#             total_advance_2016 = sum(ballot_return_count_2016, na.rm = TRUE),
#             total_advance_2014 = sum(ballot_return_count_2014, na.rm = TRUE)
#   )
# 
