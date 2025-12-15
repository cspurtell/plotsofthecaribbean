# Import relevant libraries
library(tidyverse)
library(lubridate)
library(janitor)

# Read in the datasets from data folder, then standardize for merging
pirates <- read_csv("data/pirates_subset.csv", show_col_types = FALSE) %>% clean_names()
weather <- read_csv("data/weather_subset.csv", show_col_types = FALSE) %>% clean_names()

pirates$day <- mdy(pirates$day)
weather$day <- mdy(weather$day)

pirates_clean <- pirates %>%
  transmute(
    day,
    wait   = average_wait_time_daily,
    season = factor(season),
    month  = month,
    year   = year
  ) %>%
  filter(!is.na(wait))

weather_clean <- weather %>%
  transmute(
    day,
    mean_temp = mean_temp,
    precip    = precipitation,
    dew_point = dew_point
  )

pw_merged <- pirates_clean %>%
  left_join(weather_clean, by = "day") %>%
  mutate(
    month = case_when(
      is.numeric(month) ~ month.name[month],
      TRUE ~ as.character(month)
    ),
    month = factor(month, levels = month.name),
    season = fct_drop(season)
  )

pw_merged <- pw_merged %>% 
  mutate(
    weekday = wday(day, label = TRUE, week_start = 1),
    is_weekend = weekday %in% c("Sat", "Sun")
  )

season_order <- c(
  "Winter",
  "Martin Luther King Junior Day",
  "President's Day Week",
  "Mardi Gras",
  "Spring",
  "Easter",
  "Memorial Day",
  "Summer Break",
  "4th of July",
  "September Low",
  "Fall",
  "Columbus Day",
  "Halloween",
  "Jersey Week",
  "Thanksgiving",
  "Christmas",
  "Christmas Peak"
)

pw_merged <- pw_merged %>%
  mutate(
    season = factor(season, levels = season_order)
  )
