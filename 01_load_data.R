library(tidyverse)
library(janitor)
library(lubridate)

test_state <- read_tsv("raw_data/20201019_1_avev_delivery_state.txt.gz", col_types = cols(.default = "c"))
test_county <- read_tsv("raw_data/20201019_1_avev_delivery_county.txt.gz", col_types = cols(.default = "c"))


state_2016 <- read_tsv("raw_data/2016_avev_delivery_state.txt.gz", col_types = cols(.default = "c"))
county_2016 <- read_tsv("raw_data/2016_avev_delivery_county.txt.gz", col_types = cols(.default = "c"))

test_county %>% 
  filter(countyfips == "31055") %>% 
  write_csv("sample.csv")
