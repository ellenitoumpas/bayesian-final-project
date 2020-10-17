subsampled_data <-
  read.csv(
    paste0(
      here(),
      '/OUTPUTS/SAMPLES/SUBSAMPLE_SELECTION_TRIAL/subsample_selection_trial_candidate_subsample_7.csv'
    )
  )

subsampled_data <- subsampled_data %>% as_tibble() %>%
  dplyr::mutate(dow = case_when(dow == 7 ~ 0,
                                TRUE ~ as.double(dow)))
# remove hour
subsampled_data <- subsampled_data %>%  select(-hour)

subsampled_data$pm10 <- subsampled_data$pm10 + 0.01
summary(subsampled_data$pm10)

# init linear regression model
lm1 <- lm(pm10 ~ .,
          data = subsampled_data)
lm1_smry <- summary(lm1)
write.csv(as.data.frame(lm1_smry$coefficients),
          file = 'OUT/lm1_summary.csv',
          row.names = rownames(lm1_smry$coefficients))

# remove insignificant variables for linear regression 
subsampled_data2 <- subsampled_data %>% select(-ws, -dow)
lm1b <- lm(pm10 ~ .,
          data = subsampled_data2)
lm1b_smry <- summary(lm1b)
write.csv(as.data.frame(lm1b_smry$coefficients),
          file = 'OUT/lm1b_summary.csv',
          row.names = rownames(lm1b_smry$coefficients))

# interactions model
lm2 <- lm(pm10 ~ . + .^2,
          data = subsampled_data)
 # note ws is now significant with interactions
lm2_smry <- summary(lm2)
# also R squared increased from 0.23 to 0.32
write.csv(as.data.frame(lm2_smry$coefficients),
          file = 'OUT/lm2_summary.csv',
          row.names = rownames(lm2_smry$coefficients))

# Classic linear regression without considering intercept
lm_list <- list()
r2_vec <- vector()
for (col in colnames(subsampled_data)[1:7]) {
  data_sub_sub <- subsampled_data[c(col, 'pm10')]
  lm_sum  <- summary(lm(pm10 ~ .,
                        data = data_sub_sub))
  lm_list[[col]] <- lm_sum$coefficients[2,]
  r2_vec[[col]] <- lm_sum$r.squared
}
lm_list_df <- plyr::ldply(lm_list) 
lm_list_df$R2 <- r2_vec
# Estimates do not deviate greatly (accept for ws, but there are multiple significant ws interactions)
write.csv(lm_list_df,
          file = 'OUT/lm1_individual_summary.csv')
summary(lm1)
# are individual estimates similar to the multi-lin regression model estimates?

# optimal tau linear regression
subsampled_data_opt_tau <- subsampled_data %>% select(-dow, -working_days, -ws)
lm3 <- lm(pm10 ~ .,
          data = subsampled_data_opt_tau)
lm3_smry <- summary(lm3)
lm3_smry$coefficients
write.csv(as.data.frame(lm3_smry$coefficients),
          file = 'OUT/lm3_summary.csv',
          row.names = rownames(lm3_smry$coefficients))

# interactions with optimal values
lm3b <- lm(pm10 ~ .^2,
          data = subsampled_data_opt_tau)
lm3b_smry <- summary(lm3b) 
write.csv(as.data.frame(lm3b_smry$coefficients),
          file = 'OUT/lm3b_summary.csv',
          row.names = rownames(lm3b_smry$coefficients))

# signif interactions:
# 

# why is wind speed insignificant?
ggplot(subsampled_data,
       aes(
         x = pm10,
         y = ws,
         col = working_days,
         group = working_days
       )) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Windspeed against PM10') +
  assignment_plot_theme
