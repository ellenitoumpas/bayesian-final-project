# JAGS Trial Runs

if (!requireNamespace('here'))
  install.packages('here')
library('here')
library('ggplot2')
source(here('functions', 'functions_packages.R'))
source(here('functions', 'functions_generic.R'))
source(here('functions', 'functions_bayesian.R'))
source(here('functions', 'DBDA2E-utilities.R'))
source(here('constants.R'))
source('~/bayesian-final-project/functions/functions_bayesian.R')
source(here('src', 'models', 'model.R'))
source('~/bayesian-final-project/constants.R')
source('~/bayesian-final-project/src/data/subsample.R')
source(here('src', 'data', 'train_test.R'))

graphics.off() # This closes all of R's graphics windows.
options(scipen = 999)
packages <- scan("requirements.txt", what="", sep="\n")
lock_n_load_libraries(packages)

data <- vroom::vroom(here('data', 'hour_extra_Brooklyn_2016_18.csv'))
data_cleaned <- data %>% janitor::clean_names()

data_cleaned <- data_cleaned %>%
  mutate(
    dow = factor(dow,
                 levels = days_of_weeks,
                 labels = days_of_weeks,
                 ordered = TRUE),
    winddire = factor(winddire,
                      levels = wind_directions,
                      labels = wind_directions,
                      ordered = TRUE))

data_cleaned <- data_cleaned %>% select(-id, -yn50, -yn60, -yn80, -pm10a)

# Remove incomplete rows
data_cleaned <- data_cleaned[complete.cases(data_cleaned),]

holidays <- tsibble::holiday_aus(unique(data_cleaned$years), state = 'VIC')

data_cleaned <- data_cleaned %>%
  mutate(
    deg_from_north = case_when(
      wd > 180 ~ abs(wd - 360),
      TRUE ~ wd
    ),
    pre_peak_hour = case_when(
      hour > 4 & hour < 9 ~ TRUE,
      hour > 14 & hour < 17 ~ TRUE,
      TRUE ~ FALSE
    ),
    working_days = case_when(
      weekdays == TRUE ~ TRUE,
      date_day %in% holidays$date ~ FALSE,
      weekdays == FALSE ~ FALSE
    ),
    pm10 = case_when(
      pm10 < 0 ~ 0,
      TRUE ~ pm10
    )
  )

# JAGS time! # remove hour
features <- c(
  'rf_cum_3_day',
  'temperature',
  'ws',
  'deg_from_north',
  'dow',
  'working_days',
  # 'hour',
  'pre_peak_hour',
  'pm10'
  )
predictor_variable <- 'pm10'

training_data  <- train_split(df=data_cleaned, features, target = predictor_variable)
prediction_data <- test_split(df=data_cleaned, features)

# Set up prediction values
subsampled_data <- read.csv(paste0(here(),'/OUTPUTS/SAMPLES/SUBSAMPLE_SELECTION_TRIAL/subsample_selection_trial_candidate_subsample_7.csv'))

names(subsampled_data)

subsampled_data <-subsampled_data %>%  select(-hour)

subsampled_data$pm10 <- subsampled_data$pm10 + 0.01
summary(subsampled_data$pm10)


xPred <- as.matrix(prediction_data)
colnames(xPred) <- NULL

# Set Priors here"
mus <- c(-40, # rain
         40, # temp
         0, # ws
         -40, # deg_from_north
         0, # dow
         20, # working_days
         # 0, # hour
         20) #pre_peak


vars  <- c(1/4, 
           1/2, 
           1/10, 
           1/2, 
           1/10, 
           1/2, 
           # 1/10, 
           1/2)

# Set Hyper Params here:
nChains <- 3
burnInSteps <- 500
adaptSteps <- 500
thinningSteps <- 5

use_parameters <- list()
# update run model again with 1000 burn in steps
use_parameters[['number_chains']] <- nChains
use_parameters[['burn_in_steps']] <- burnInSteps
use_parameters[['number_adaptation_steps']] <- adaptSteps
use_parameters[['thinning_steps']] <- thinningSteps

trial_type <- glue::glue('model2_001_gamma_gamma_c{nChains}_b{burnInSteps}_a{adaptSteps}_t{thinningSteps}')

prepare_JAGS_model(mu_list = mus,
                   var_list = vars,
                   num_predictions = nrow(xPred))


initial_values_list <- get_initial_values(subsampled_data, method = "likelihood-mean", pred = "pm10")
# Run JAGS model with candidate sample

if (hasnt_run(trial_type)) {

  returned_values <- setup_run_JAGS_trial(data = subsampled_data, 
                                          predictor = "pm10", 
                                          predictions = xPred, 
                                          mu_list = mus,
                                          var_list = vars, 
                                          initial_values = initial_values_list, 
                                          params = use_parameters,
                                          par_trial_name = trial_type, 
                                          num_predictions = nrow(xPred))
  graphics.off()

}


