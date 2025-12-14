library(tidyverse)
library(lubridate)

# Importing datasets and models
source("01_data_prep.R")
source("02_modeling.R")

# Create a directory to store outputted figures
if (!dir.exists("figures")) dir.create("figures")

# Fig 1: Time Series of Daily Wait Times
ggplot(pw_merged, aes(x = day, y = wait)) +
  geom_line(alpha = 0.6) +
  labs(
    x = "Date",
    y = "Daily average wait time (minutes)"
  )

# Fig 2: Seasonal Means w/ Confidence Intervals
pw_merged %>%
  group_by(season) %>%
  summarise(
    mean_wait = mean(wait),
    se = sd(wait) / sqrt(n())
  ) %>%
  ggplot(aes(x = season, y = mean_wait)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_wait - 1.96 * se,
                    ymax = mean_wait + 1.96 * se),
                width = 0.2) +
  labs(
    x = "Season",
    y = "Mean daily wait time (minutes)"
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

