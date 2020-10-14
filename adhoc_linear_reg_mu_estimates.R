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


lm1 <- lm(pm10 ~ .,
          data = subsampled_data)
summary(lm1)


lm2 <- lm(pm10 ~ .^2,
          data = subsampled_data)
summary(lm2)

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

lm_list_df
# are individual estimates similar to the multi-lin regression model estimates?

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
