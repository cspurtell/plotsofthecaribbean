# Import relevant libraries
library(tidyverse)
library(lubridate)
library(janitor)

# Read in the datasets from data/
pirates_raw <- read_csv("data/pirates_subset.csv", show_col_types = FALSE)
pirates <- pirates_raw %>% clean_names()
weather_raw <- read_csv("data/weather_subset.csv", show_col_types = FALSE)
weather <- weather_raw %>% clean_names()
