library(tidyverse)
library(lubridate)

# Importing datasets and models
source("01_data_prep.R")
source("02_modeling.R")

# Create a directory to store outputted figures
if (!dir.exists("figures")) dir.create("figures")

## Note: Not all of these figs are used in the report, but I included them for experimentation sake ##

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

# Fig 3: Residuals vs. Fitted
plot(m_weather, which = 1)

# Fig 4: Observed vs. Predicted by Season
pw_merged %>%
  mutate(pred = predict(m_weather)) %>%
  ggplot(aes(x = pred, y = wait)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ season) +
  geom_abline(intercept = 0, slope = 1)

# Fig 5: Partial Effect of Temp vs. Wait Time
ggplot(pw_merged, aes(x = mean_temp, y = wait)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean daily temperature",
    y = "Daily average wait time"
  )

# Fig 6: Weekends vs. Weekdays
ggplot(pw_merged, aes(x = is_weekend, y = wait)) +
  geom_boxplot() +
  labs(
    x = "Weekend",
    y = "Daily average wait time (minutes)"
  )

# Fig 7: Heatmap of average wait by month x weekday
pw_merged %>%
  mutate(
    month = month(day, label = TRUE),
    weekday = wday(day, label = TRUE)
  ) %>%
  group_by(month, weekday) %>%
  summarise(mean_wait = mean(wait), .groups = "drop") %>%
  ggplot(aes(x = weekday, y = month, fill = mean_wait)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    x = "Day of week",
    y = "Month",
    fill = "Avg wait (min)"
  )

