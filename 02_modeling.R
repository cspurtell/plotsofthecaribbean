library(tidyverse)
library(broom)
library(lubridate)
library(car)

# Creating merged dataset
source("01_data_prep.R")

# Building model based purely on "crowd calendar" vs. adding in weather data
m_base <- lm(wait ~ season + year + is_weekend, data = pw_merged)
summary(m_base)

m_weather <- lm(wait ~ season + year + is_weekend + mean_temp + precip + dew_point, data = pw_merged)
summary(m_weather)

# Adding weather variables improves the model!

anova(m_base, m_weather)

vif(m_weather)

pw_merged %>%
  select(mean_temp, dew_point, precip) %>%
  cor(use = "complete.obs")