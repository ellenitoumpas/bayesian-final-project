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
source(here('src', 'models', 'model.R'))
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
  # 'pre_peak_hour',
  'pm10'
  )
predictor <- 'pm10'

training_data  <- train_split(df=data_cleaned, features, target = predictor_variable)
ground_truths <- test_split(df=data_cleaned, features, target = predictor_variable)['pm10']
prediction_data <- test_split(df=data_cleaned, features)

# Set up prediction values
subsampled_data <- read.csv(paste0(here(),'/OUTPUTS/SAMPLES/SUBSAMPLE_SELECTION_TRIAL/subsample_selection_trial_candidate_subsample_7.csv'))

names(subsampled_data)

# grab best priors so far, restructure days of week to sunday as 0
subsampled_data <- subsampled_data %>% as_tibble() %>%  
  dplyr::mutate(dow = case_when(
    dow == 7 ~ 0,
    TRUE ~ as.double(dow)
))

# Remove unwanted variables
subsampled_data <-subsampled_data %>%  select(-hour)

subsampled_data$pm10 <- subsampled_data$pm10 + 0.01
summary(subsampled_data$pm10)


xPred <- as.matrix(prediction_data)
colnames(xPred) <- NULL

#------------------------------------------------------------------------------#
# Set Priors here:
mus <- c(-1, # rain
         1, # temp
         1, # ws
         -1, # deg_from_north
         1, # dow
         9, # working_days
         # 0, # hour
         10) #pre_peak


vars  <- c(1/2, # rain
           1/2, # temp
           1/1000, # ws
           1/2, # deg_from_north
           1/1000, # dow
           1/100, # working_days
           # 1/10, # hour
           1/100) #pre_peak

# Set Hyper Params here:
nChains <- 3
burnInSteps <- 500
adaptSteps <- 500
thinningSteps <- 5

# Set model name:
model_name <- 'model0inf5c'

#------------------------------------------------------------------------------#

trial_type <- glue::glue('{model_name}_001_gamma_gamma_c{nChains}_b{burnInSteps}_a{adaptSteps}_t{thinningSteps}')

# Create Init values list:
initial_values <- get_initial_values(subsampled_data, method = "likelihood-mean", pred = "pm10")

# Setting up and saving trial data frame info
trial_info <- as.data.frame(matrix(ncol = 0, nrow = 1))

trial_info$trial_name <- trial_type

for(i in 1:length(mus))(trial_info[[paste0('mu',stringr::str_pad(as.character(i), width = 2, side = "left", pad = "0"))]] <- mus[i])
for(i in 1:length(vars))(trial_info[[paste0('var',stringr::str_pad(as.character(i), width = 2, side = "left", pad = "0"))]] <- vars[i])

trial_info$number_chains <- nChains
trial_info$number_adaptation_steps <- adaptSteps
trial_info$burn_in_steps <- burnInSteps
trial_info$thinning_steps <- thinningSteps

for(i in 1:length(initial_values))(trial_info[[paste0('initial_values_',stringr::str_pad(as.character(i), width = 2, side = "left", pad = "0"))]] <- initial_values[i])

trial_info$duration = 0

# Split independant and dependant variables
y_data <- subsampled_data[[predictor]]
x_data <- as.matrix(subsampled_data[,!(colnames(subsampled_data) %in% predictor)])

# Specify data list for JAGS
dataList <- list(
  x = x_data ,
  y = y_data ,
  xPred = xPred,
  Nx = dim(x_data)[2] ,
  Ntotal = dim(x_data)[1]
)

# Zero intercept?:
zero_intercept = T

# Prepare JAGS model
prepare_JAGS_model(mu_list = mus,
                   var_list = vars,
                   num_predictions = nrow(xPred),
                   zero_intercept = zero_intercept)

# Set up monitoring parameters
parameters <- c("beta0", "beta", "zbeta0", "zbeta", "tau", "zVar", "pred")

if (zero_intercept == T) {
  parameters <- parameters[!parameters %in% c('beta0', 'zbeta0')]
}


# Set up BLANK comparison values
compVal <- data.frame("beta0" = NA, 
                      "beta[1]" = NA, 
                      "beta[2]" = NA,  
                      "beta[3]" = NA, 
                      "beta[4]" =  NA,  
                      "beta[5]" =  NA, 
                      "beta[6]" =  NA, 
                      "beta[7]" =  NA,
                      # "beta[8]" =  NA, 
                      "tau" = NA , 
                      check.names = FALSE)

if (zero_intercept == T) {
  compVal <- compVal %>% select(-beta0)
}

# Set initial values
if(!is.null(initial_values)){
  initsList <- list(
    zbeta0 = initial_values[1],
    zbeta = initial_values[seq(from = 2, to = length(initial_values)-1, by = 1)],
    zVar = initial_values[length(initial_values)]
  )
} else {
  initsList <- NULL
}

if (zero_intercept == T) {
  initsList$zbeta0 <- NULL
}

if (hasnt_run(trial_type)) {
  # Run JAGS
  start_time <- proc.time()
  runJagsOut <- runjags::run.jags(method = "parallel",
                                  model = 'TEMPmodel.txt',
                                  monitor = c("beta", "zbeta", "tau", "zVar", "pred"),
                                  data = dataList,
                                  inits = initsList ,
                                  n.chains = nChains,
                                  adapt = adaptSteps,
                                  burnin = burnInSteps ,
                                  sample = ceiling((burnInSteps * thinningSteps)/ nChains) ,
                                  thin = thinningSteps , 
                                  summarise = F, 
                                  plots = F)
  time <- proc.time() - start_time
  trial_info$duration <- time[3]
  saveRDS(runJagsOut,
          glue::glue('OUTPUTS/RData/{trial_type}.RDS'))
  
  write_csv(trial_info,
            glue::glue('OUTPUTS/TRIAL_INFO/{trial_type}.csv'))

# runJagsOut <- readRDS('OUTPUTS/RData/model0inf2_003_gamma_gamma_c3_b500_a500_t5.RDS')

coda_samples <- coda::as.mcmc.list(runJagsOut)

subDir <- paste0(here(),'/OUTPUTS/IMAGES/BETA_DIAGNOSTICS/',toupper(trial_type),"/")

dir.create(subDir)

diagMCMC(coda_samples, parName = 'beta[1]', saveName = paste0(subDir, 'beta1'))
diagMCMC(coda_samples, parName = 'beta[2]', saveName = paste0(subDir, 'beta2'))
diagMCMC(coda_samples, parName = 'beta[3]', saveName = paste0(subDir, 'beta3'))
diagMCMC(coda_samples, parName = 'beta[4]', saveName = paste0(subDir, 'beta4'))
diagMCMC(coda_samples, parName = 'beta[5]', saveName = paste0(subDir, 'beta5'))
diagMCMC(coda_samples, parName = 'beta[6]', saveName = paste0(subDir, 'beta6'))
diagMCMC(coda_samples, parName = 'beta[7]', saveName = paste0(subDir, 'beta7'))
diagMCMC(coda_samples, parName = 'tau', saveName = paste0(subDir, 'tau'))

diagMCMC(coda_samples, parName = 'pred[1]', saveName = paste0(subDir, 'pred1'))
diagMCMC(coda_samples, parName = 'pred[10]', saveName = paste0(subDir, 'pred10'))
diagMCMC(coda_samples, parName = 'pred[20]', saveName = paste0(subDir, 'pred20'))
diagMCMC(coda_samples, parName = 'pred[30]', saveName = paste0(subDir, 'pred30'))

# test ground truths
summaryInfo <- smryMCMC_HD(coda_samples,
                           compVal)

summaryInfo_df <- as.data.frame(summaryInfo)
predictions_visual <- 
  tibble(
    predictions = summaryInfo_df[str_detect(rownames(summaryInfo_df), 'pred'), "Mean"],
    actuals = ground_truths$pm10)

p <- ggplot(predictions_visual, aes(x = predictions, y = actuals)) +
  geom_point(col = 'orange', alpha = 0.5, size = 2) +
  geom_smooth(method = 'lm', col = 'orange', fill = 'orange', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, col = 'red', lwd = 1, alpha = 0.5) +
  assignment_plot_theme +
  labs(
    title = 'Actuals vs Predictions',
    subtitle = paste0(trial_type, '\nRed Line is 1 to 1'))

p
# dir.create('OUTPUTS/IMAGES/PREDICTIONS')
ggsave(glue::glue('OUTPUTS/IMAGES/PREDICTIONS/{trial_type}.png'),
       p,
       dpi = 200)

} else {
  stop('Trial has been run!')
}


