library(tidyverse)
library(lubridate)
library(gt)
library(modelsummary)
library(broom)
library(dplyr)
library(knitr)

# Importing datasets and models
source("01_data_prep.R")
source("02_modeling.R")

# Create a directory to store outputted figures
if (!dir.exists("figures")) dir.create("figures")

## Note: Not all of these figs are used in the report, but I included them for experimentation sake ##

# Fig A2: Time Series of Daily Wait Times (in appendix)
pw_ts2 <- pw_merged %>%
  select(day, wait) %>%
  arrange(day) %>%
  complete(day = seq(min(day), max(day), by = "day"))

fig1_time_series <- ggplot(pw_ts2, aes(x = day, y = wait)) +
  geom_line(na.rm = TRUE, alpha = 0.6) +
  labs(
    x = "Date",
    y = "Daily average wait time (min)"
  ) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 9),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    plot.margin = margin(10, 10, 25, 10)
  )
fig1_time_series

ggsave(
  filename = "figures/figA2_time_series.png",
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
fig2_season_ci

ggsave("figures/fig2_season_ci.png", fig2_season_ci, width = 8, height = 4.5, dpi = 300)

# Fig 2.1: Improved version of above (used in report)
fig2_boxplot<- ggplot(pw_merged, aes(x = season, y = wait)) +
  geom_boxplot(
    outlier.alpha = 0.0,
    fill = "grey85",
    color = "grey30"
  ) +
  scale_x_discrete(labels = c(
    "Winter" = "Winter",
    "Martin Luther King Junior Day" = "MLK Jr Day",
    "President's Day Week" = "Presidents’ Day",
    "Mardi Gras" = "Mardi Gras",
    "Spring" = "Spring",
    "Easter" = "Easter",
    "Memorial Day" = "Memorial Day",
    "Summer Break" = "Summer",
    "4th of July" = "4th of July",
    "September Low" = "September Low",
    "Fall" = "Fall",
    "Columbus Day" = "Columbus Day",
    "Halloween" = "Halloween",
    "Jersey Week" = "Jersey Week",
    "Thanksgiving" = "Thanksgiving",
    "Christmas" = "Christmas",
    "Christmas Peak" = "Christmas Peak"
  )) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = "Seasonal period",
    y = "Daily average wait time (min)"
  ) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 9),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    plot.margin = margin(10, 10, 25, 10)
  )

fig2_boxplot

ggsave("figures/fig1_boxplot.png", fig2_boxplot, width = 8, height = 4.5, dpi = 300)

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
fig4_obs_pred_season

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
fig5_temp

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

# Fig 8: Sample of dataset
set.seed(123)

sample_tbl <- pw_merged %>%
  select(
    day,
    wait,
    season,
    is_weekend,
    mean_temp,
    precip,
    dew_point
  ) %>%
  group_by(season) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  arrange(day)
sample_tbl <- sample_tbl %>% slice(1:8)

table_fig <- sample_tbl %>%
  gt() %>%
  tab_header(
    title = "Sample of merged dataset",
    subtitle = "Daily wait times, calendar variables, and weather conditions"
  ) %>%
  fmt_number(
    columns = c(wait, mean_temp, precip, dew_point),
    decimals = 1
  )

gtsave(table_fig, "figures/figA1_sample_data_table.png")

# Fig 9: Model Summary
summary(m_weather)

# Fig 10: ANOVA Table
model_comp <- tibble(
  Model = c("Baseline", "Weather"),
  Predictors = c(
    "Season + Year + Weekend",
    "+ Temperature + Precipitation + Dew Point"
  ),
  `Adj. R²` = c(
    round(summary(m_base)$adj.r.squared, 3),
    round(summary(m_weather)$adj.r.squared, 3)
  ),
  `Residual SE` = c(
    round(summary(m_base)$sigma, 2),
    round(summary(m_weather)$sigma, 2)
  )
)

anova_res <- anova(m_base, m_weather)

anova_note <- paste0(
  "Nested ANOVA: F = ",
  round(anova_res$F[2], 2),
  ", p = ",
  format.pval(anova_res$`Pr(>F)`[2], digits = 3)
)

fig2_table <- model_comp %>%
  gt() %>%
  tab_header(
    title = "Model comparison for daily wait times",
    subtitle = anova_note
  ) %>%
  cols_align(
    align = "center",
    everything()
  ) %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = "Baseline model includes calendar variables only; weather model adds daily weather conditions."
  )

gtsave(fig2_table, "figures/figure_2_model_comparison.png")
