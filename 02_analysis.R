library(tidyverse)
library(janitor)
library(lubridate)

# load processed files from step 01
state_latest <- readRDS("processed_data/state_latest.rds")
county_latest <- readRDS("processed_data/county_latest.rds")
state_2016 <- readRDS("processed_data/state_2016.rds")
county_2016 <- readRDS("processed_data/county_2016.rds")
