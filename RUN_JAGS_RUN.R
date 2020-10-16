# JAGS Trial Runs

if (!requireNamespace('here'))
  install.packages('here')
library('here')
here()
library('ggplot2')
source(here('functions', 'functions_packages.R'))
source(here('functions', 'functions_generic.R'))
source(here('functions', 'functions_bayesian.R'))
source(here('functions', 'DBDA2E-utilities.R'))
source(here('constants.R'))
source(here('src', 'models', 'model.R'))
source(here('src', 'data', 'subsample.R'))
source(here('src', 'data', 'train_test.R'))
graphics.off() # This closes all of R's graphics windows.
options(scipen = 999)
packages <- scan("requirements.txt", what = "", sep = "\n")
lock_n_load_libraries(packages)


fullsample_data <- clean_existing_data()
subsample_data <- read.csv(paste0(here(),'/OUTPUTS/SAMPLES/SUBSAMPLE_SELECTION_TRIAL/subsample_selection_trial_candidate_subsample_7.csv'))


# A bit more cleaning

# grab best priors so far, restructure days of week to sunday as 0
fullsample_data <- fullsample_data %>% mutate(dow = case_when(dow == 7 ~ 0, TRUE ~ as.double(dow)))
subsample_data <- subsample_data %>% mutate(dow = case_when(dow == 7 ~ 0, TRUE ~ as.double(dow)))

# Add 0.01
fullsample_data$pm10 <- fullsample_data$pm10 + 0.01
subsample_data$pm10 <- subsample_data$pm10 + 0.01

# SEED SET FOR PREDICTION VALUES
set.seed(1234)
prediction_indices <- c(sample(1:nrow(subsample_data), 10, replace = FALSE))
training_data <- subsample_data[-c(prediction_indices), features]
prediction_data <- subsample_data[c(prediction_indices), features] %>% select(-predictor)
ground_truths <- subsample_data[c(prediction_indices), ] %>% select(predictor)

#------------------------------------------------------------------------------#


trial_type <- glue::glue('{model_name}_gamma_gamma_c{nChains}_b{burnInSteps}_a{adaptSteps}_t{thinningSteps}')

# Create Init values list:
initial_values <- get_initial_values(training_data, method = "likelihood-mean", pred = "pm10")

# TODO: later: initial_values should be output as a constant, to avoid change

# Setting up and saving trial data frame info
trial_info <- as.data.frame(matrix(ncol = 0, nrow = 1))
trial_info$trial_name <- trial_type

# populate trial_info
for(i in 1:length(mus))(trial_info[[paste0('mu',stringr::str_pad(as.character(i), width = 2, side = "left", pad = "0"))]] <- mus[i])
for(i in 1:length(vars))(trial_info[[paste0('var',stringr::str_pad(as.character(i), width = 2, side = "left", pad = "0"))]] <- vars[i])

trial_info$number_chains <- nChains
trial_info$number_adaptation_steps <- adaptSteps
trial_info$burn_in_steps <- burnInSteps
trial_info$thinning_steps <- thinningSteps

for(i in 1:length(initial_values))(trial_info[[paste0('initial_values_',stringr::str_pad(as.character(i), width = 2, side = "left", pad = "0"))]] <- initial_values[i])
trial_info$duration = 0

# Split independant and dependant variables
# TODO: we still need to only split the data once here, not in two places
y_data <- training_data[[predictor]]
x_data <- as.matrix(training_data %>% select(-predictor))
xPred <- as.matrix(prediction_data)
colnames(xPred) <- NULL

# Specify data list for JAGS
dataList <- list(
  x = x_data,
  y = y_data,
  xPred = xPred,
  Nx = dim(x_data)[2] ,
  Ntotal = dim(x_data)[1]
)



########## SET TRUE/ FALSE HERE

# Zero intercept?:
zero_intercept = TRUE

########## RUN PARAM SETTING

# Prepare JAGS model
prepare_JAGS_model(mu_list = mus,
                   var_list = vars,
                   num_predictions = nrow(xPred),
                   zero_intercept = zero_intercept)

if (zero_intercept == TRUE) {
  parameters <- parameters[!parameters %in% c('beta0', 'zbeta0')]
}

# Set up BLANK comparison values
compVal <- data.frame("beta0" = NA)
for(beta in 1:dim(x_data)[2]){ compVal[paste0("beta[",beta,"]")] <- NA }
compVal[paste0("tau")] <- NA

if (zero_intercept == TRUE) {
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

if (zero_intercept == TRUE) {
  initsList$zbeta0 <- NULL
}


########## RUN THE MODEL


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
                                  summarise = FALSE,
                                  plots = FALSE)
  
  time <- proc.time() - start_time
  trial_info$duration <- time[3]
  saveRDS(runJagsOut, glue::glue('OUTPUTS/RData/{trial_type}.RDS'))
  write_csv(trial_info, glue::glue('OUTPUTS/TRIAL_INFO/{trial_type}.csv'))
  
  # TODO: why is this commented out?
  # runJagsOut <- readRDS('OUTPUTS/RData/model0inf2_003_gamma_gamma_c3_b500_a500_t5.RDS')
  
  coda_samples <- coda::as.mcmc.list(runJagsOut)
  
  
  # Prepare subdirection for image capture
  subDir <- paste0(here(),'/OUTPUTS/IMAGES/BETA_DIAGNOSTICS/',toupper(trial_type),"/")
  dir.create(subDir)
  
  # Capture diagMCMC
  prediction_indices <- 1:nrow(xPred)
  create_diags(coda_samples, prediction_indices, subDir)
  plotMCMC_HD(codaSamples = coda_samples,  data = subsample_data, xName = colnames(dataList$x), 
              yName = 'pm10', compVal = compVal, saveName = trial_type, 
              number_predictions = prediction_indices, binded_intercept = zero_intercept)
  
  
  # Close diags
  graphics.off()
  
  # Capture summary dataframes
  summaryInfo <- smryMCMC_HD(coda_samples, compVal, saveName = trial_type)
  summaryInfo_df <- as.data.frame(summaryInfo)
  
  
  # Test ground truths
  pred_vis <- predictions_visual(summaryInfo_df, trial_type)
  pred_vis
  
  p_sub <- prediction_density_overlay(summaryInfo_df, x_data, y_data, trial_type, 'subsample') # Actual values from subsample compared to model values
  p_sub
  
  # Test model on full dataset 
  x_data_fullsample <- fullsample_data[, features] %>% select(-predictor) %>% as.matrix()
  y_data_fullsample <- fullsample_data[[predictor]]
  p_full <- prediction_density_overlay(summaryInfo_df, x_data_fullsample, y_data_fullsample, trial_type, 'fullsample') # Actual values from fullsample compared to model values
  p_full

} else {
  
  stop('Trial has been run!')
  
}


