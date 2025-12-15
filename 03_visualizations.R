library(tidyverse)
library(lubridate)

# Importing datasets and models
source("01_data_prep.R")
source("02_modeling.R")

# Create a directory to store outputted figures
if (!dir.exists("figures")) dir.create("figures")

## Note: Not all of these figs are used in the report, but I included them for experimentation sake ##

# Fig 1: Time Series of Daily Wait Times
fig1_time_series <- ggplot(pw_merged, aes(x = day, y = wait)) +
  geom_line(alpha = 0.6) +
  labs(
    x = "Date",
    y = "Daily average wait time (minutes)"
  )

ggsave(
  filename = "figures/fig1_time_series.png",
  plot = fig1_time_series,
  width = 8,
  height = 4.5,
  dpi = 300
)

# Fig 2: Seasonal Means w/ Confidence Intervals
fig2_season_ci <- pw_merged %>%
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

ggsave("figures/fig2_season_ci.png", fig2_season_ci, width = 8, height = 4.5, dpi = 300)

# Fig 2.1: Improved version of above
fig2_boxplot<- ggplot(pw_merged, aes(x = season, y = wait)) +
  geom_boxplot(
    outlier.alpha = 0.2,
    fill = "grey85",
    color = "grey30"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = "Seasonal period",
    y = "Daily average wait time (minutes)"
  )

ggsave("figures/fig2_boxplot.png", fig2_boxplot, width = 8, height = 4.5, dpi = 300)

# Fig 3: Residuals vs. Fitted
png("figures/fig3_residuals_vs_fitted.png", width = 800, height = 600)
plot(m_weather, which = 1)
dev.off()

# Fig 4: Observed vs. Predicted by Season
fig4_obs_pred_season <- pw_merged %>%
  mutate(pred = predict(m_weather)) %>%
  ggplot(aes(x = pred, y = wait)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ season) +
  geom_abline(intercept = 0, slope = 1)

ggsave("figures/fig4_obs_pred_by_season.png",
       fig4_obs_pred_season, width = 9, height = 6, dpi = 300)

# Fig 5: Partial Effect of Temp vs. Wait Time
fig5_temp <- ggplot(pw_merged, aes(x = mean_temp, y = wait)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean daily temperature",
    y = "Daily average wait time"
  )

ggsave("figures/fig5_temp_vs_wait.png", fig5_temp, width = 6.5, height = 5, dpi = 300)

# Fig 6: Weekends vs. Weekdays
fig6_weekend <- ggplot(pw_merged, aes(x = is_weekend, y = wait)) +
  geom_boxplot() +
  labs(
    x = "Weekend",
    y = "Daily average wait time (minutes)"
  )

ggsave("figures/fig6_weekend.png", fig6_weekend, width = 5, height = 4, dpi = 300)

# Fig 7: Heatmap of average wait by month x weekday
fig7_heatmap <- pw_merged %>%
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

ggsave("figures/fig7_month_weekday_heatmap.png",
       fig7_heatmap, width = 6.5, height = 5.5, dpi = 300)
