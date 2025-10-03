# This script creates:
# 1. RF model's performance data, 
# 2. additional tables for calculating the pooled savings (2 options),
# Author: Mahmud Aveek, David Rosenberg
# Date: 10/02/2025
# Runtime: 30 seconds

library(yardstick)

set.seed(123)
split <- initial_split(td, prop = 0.7, strata = label)
train_data <- training(split)
test_data  <- testing(split)

rf_model <- rand_forest(mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(label ~ ., data = train_data)

predictions <- predict(rf_model, test_data) %>%
  bind_cols(test_data)

metrics <- yardstick::metrics(predictions, truth = label, estimate = .pred_class)

################################################################################

# Appendix Table: A1
# Total Pooled saving calculation

####################
# option 1
df_test_weekly_volumetric  %>% 
  mutate(weekly_volume_liter = weekly_volume * 3.78541,
         weekly_budget_liter = budgetGal * 3.78541) %>%
  select(SiteID, week, pre_post, weekly_volume_liter, weekly_budget_liter) %>%
  group_by(SiteID, pre_post) %>%
  summarise(
    weeks_obs = n(),
    mean_weekly_liter = mean(weekly_volume_liter, na.rm = TRUE),
    mean_weekly_budget_liter = mean(weekly_budget_liter, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  tidyr::pivot_wider(
    names_from = pre_post,
    values_from = c(weeks_obs, mean_weekly_liter,mean_weekly_budget_liter),
    names_sep = "."
  ) %>%
  mutate(
    pre_audit_use = weeks_obs.pre * mean_weekly_liter.pre,
    post_audit_use = weeks_obs.post *mean_weekly_liter.post
  ) -> total_and_mean_weeklyUse_budget_table_liter_with_pooled_savings_option1



total_irr_water_use_pre <- sum(total_and_mean_weeklyUse_budget_table_liter_with_pooled_savings_option1$pre_audit_use)
total_irr_water_use_post <- sum(total_and_mean_weeklyUse_budget_table_liter_with_pooled_savings_option1$post_audit_use)

avg_weekly_water_use_pre <- total_irr_water_use_pre/sum(total_and_mean_weeklyUse_budget_table_liter_with_pooled_savings_option1$weeks_obs.pre)
avg_weekly_water_use_post <- total_irr_water_use_post/sum(total_and_mean_weeklyUse_budget_table_liter_with_pooled_savings_option1$weeks_obs.post)

savings_per_week <- avg_weekly_water_use_pre -avg_weekly_water_use_post
pooled_savings <- savings_per_week*length(row_number(total_and_mean_weeklyUse_budget_table_liter_with_pooled_savings_option1))

####################
# Option 2
df_test_weekly_volumetric  %>% 
  mutate(weekly_volume_liter = weekly_volume * 3.78541,
         weekly_budget_liter = budgetGal * 3.78541) %>%
  select(SiteID, week, pre_post, weekly_volume_liter, weekly_budget_liter) %>%
  group_by(SiteID, pre_post) %>%
  summarise(
    weeks_obs = n(),
    mean_weekly_liter = mean(weekly_volume_liter, na.rm = TRUE),
    mean_weekly_budget_liter = mean(weekly_budget_liter, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  tidyr::pivot_wider(
    names_from = pre_post,
    values_from = c(weeks_obs, mean_weekly_liter,mean_weekly_budget_liter),
    names_sep = "."
  ) %>%
  mutate(
    W_star = pmin(weeks_obs.pre, weeks_obs.post),
    delta_per_week =  mean_weekly_liter.pre - mean_weekly_liter.post,
    S_i = delta_per_week * W_star # gallons
  ) -> total_and_mean_weeklyUse_budget_table_liter_with_pooled_savings_option2
################################################################################