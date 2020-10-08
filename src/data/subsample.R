library('here')
library('dplyr')
source(here('functions', 'functions_generic.R'))
# source(here('functions', 'functions_packages.R'))
# lock_n_load_libraries(packages)
#' @title Subsample
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

subsample <- function(df){

  # TODO: make the function do something. It currently returns the original dataframe
  return(df)
  
}

# Run the subsample size trials
#' Title
#'
#' @param trial_df 
#' @param row_num 
#' @param full_sample 
#' @param trial_name 
#'
#' @return
#' @export
#' 
#' @examples
run_subsample_trial <- function(trial_df, row_num, full_sample, trial_name){
  
  # Set randomised seed
  set.seed(trial_df[row_num,'seed'])
  
  # Create candidate subsample, 
  candidate_sample_selected <- mv_data_subsampling(data = full_sample, n_sample = trial_df[row_num,'sampling_size'])
  print('candidate_sample_selected')
  
  # Saving directory
  subDir <- paste0(here(),'/OUTPUTS/SAMPLES/',toupper(trial_name),'_TRIAL/')
  
  # If directory doesn't exist create directory
  if(!file.exists(subDir))(dir.create(file.path(subDir), showWarnings = FALSE))
  
  # Store candidate subsample selected
  write.csv(candidate_sample_selected, paste0(subDir,trial_name,'_trial_candidate_subsample_', stringr::str_pad(row_num, width = "0", side = "left", pad = "0"),'.csv'), row.names = FALSE)
  
  
  # Run ks.test on paired variables
  for (col in colnames(full_sample)){
    
    print(paste0('Testing ',col))
    
    ks_results <- stats::ks.test(candidate_sample_selected[[col]], full_sample[[col]])
    trial_df[row_num, paste0(col,"_p_value")] <- ks_results$p.value
    
    print(paste0('\n'))
    
  }
  
  returned_values <- list(candidate_sample_selected, trial_df)
  names(returned_values) <- c('candidate_sample_selected', 'trial_df')
  
  return(returned_values)
  
}

#' @title Subsample size
#' @description
#'
#' @export
#' '/OUTPUTS/TRIAL_INFO/',trial_type,'_trials.csv'
#' '/OUTPUTS/SAMPLES/',toupper(trial_name),'_TRIAL/'

subsample_size <- function(training_data, trial_type){

if(hasnt_run(trial_type)){

  # Set seeds
  seed_value <- sample(1:10000, 20, replace=F)
  write.csv(seed_value, paste0(here(),"/OUTPUTS/SEEDS/",trial_type,"_seeds.csv"), row.names = FALSE)
  
  # Set up sample size trial values
  sample_size_list <- c(100, 500, 1000, 2000, 2500, 5000)
  
  # Set up trial df and set up columns
  subsample_size_trials <- expand.grid(sampling_size = sample_size_list, seed = seed_value)
  subsample_size_trials[c(paste0(colnames(training_data),"_p_value"), "duration")] <- 0
  subsample_size_trials['trial_number'] <- 1:nrow(subsample_size_trials)

  # Added more trials later
  sample_size_list_more <- c(7500, 10000)
  subsample_size_trials_more <- expand.grid(sampling_size = sample_size_list_more, seed = seed_value)
  subsample_size_trials_more[c(paste0(colnames(training_data),"_p_value"), "duration")] <- 0
  subsample_size_trials_more['trial_number'] <- (1+nrow(subsample_size_trials)):(nrow(subsample_size_trials_more)+nrow(subsample_size_trials))
  old_n <- nrow(subsample_size_trials)
  subsample_size_trials <- rbind(subsample_size_trials,subsample_size_trials_more)
  
  
  # Run Subsampling trials
  for(row in 1:nrow(subsample_size_trials)){
    
    print(paste0('Running trial ',row))
    
    # Running subsample size trial
    returned_values <- run_subsample_trial(trial_df = subsample_size_trials, row_num = row, 
                                                              full_sample = training_data, trial_name = 'subsampling_size')
    
    returned_candidate_sample_selected <- returned_values$candidate_sample_selected
    subsample_size_trials <- returned_values$trial_df
    print('Candidate sample selected')
    
    # Deal with 0 pm10 levels
    returned_candidate_sample_selected$pm10 <- returned_candidate_sample_selected$pm10 + 0.1 
    
    # Select initial values based on candidate subsample
    sd_rf_cum_3_day <-   sd(returned_candidate_sample_selected$rf_cum_3_day)
    sd_temperature <-   sd(returned_candidate_sample_selected$temperature)
    sd_ws <-   sd(returned_candidate_sample_selected$ws)
    sd_deg_from_north <-   sd(returned_candidate_sample_selected$deg_from_north)
    sd_dow <-   sd(returned_candidate_sample_selected$dow)
    sd_working_days <-   sd(returned_candidate_sample_selected$working_days)
    sd_hour <-   sd(returned_candidate_sample_selected$hour)
    sd_pre_peak_hour <-   sd(returned_candidate_sample_selected$pre_peak_hour)
    sd_pm10 <-   sd(returned_candidate_sample_selected$pm10)
    
    initial_values_list <- c(0.1,
                             mean(returned_candidate_sample_selected$rf_cum_3_day)/sd_rf_cum_3_day, 
                             mean(returned_candidate_sample_selected$temperature)/sd_temperature, 
                             mean(returned_candidate_sample_selected$ws)/sd_ws, 
                             mean(returned_candidate_sample_selected$deg_from_north)/sd_deg_from_north, 
                             mean(returned_candidate_sample_selected$dow)/sd_dow, 
                             mean(returned_candidate_sample_selected$working_days)/sd_working_days, 
                             mean(returned_candidate_sample_selected$hour)/sd_hour,
                             mean(returned_candidate_sample_selected$pre_peak_hour)/sd_pre_peak_hour,
                             0.01)

    # Run JAGS model with candidate sample
    returned_values <- run_subsample_size_JAGS_trial(data = returned_candidate_sample_selected, 
                                                     predictor = "pm10", 
                                                     predictions = xPred, 
                                                     mu_list = mu, 
                                                     var_list = var,
                                                     initial_values = initial_values_list, 
                                                     params = use_parameters, 
                                                     trial_num = row)
    print('JAGS model run')
    
    # Save time elapsed for the model - Store
    subsample_size_trials[row, "duration"] <- returned_values['time_elapsed']
    write.csv(subsample_size_trials, paste0(here(),'/OUTPUTS/TRIAL_INFO/',trial_type,'_trials.csv'), row.names = FALSE)
  
    # Save RData 
    save.image(file = "subsample_size_trials.RData")
    
    # Reset
    graphics.off()
    
  }
  
  subsample_size_trials <- subsample_size_trials %>% 
    mutate(mean_p_value = rowMeans(subsample_size_trials[c('rf_cum_3_day_p_value', 'temperature_p_value', 'ws_p_value', 'deg_from_north_p_value', 'dow_p_value', 
                                                           'working_days_p_value', 'hour_p_value', 'pre_peak_hour_p_value', 'pm10_p_value')]))
   
  write.csv(subsample_size_trials, paste0(here(),'/OUTPUTS/TRIAL_INFO/',trial_type,'_trials.csv'), row.names = FALSE)

} else {
  
  print('Already run')
  
}
  
}

#' @title Subsample selection
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

subsample_selection <- function(data_cleaned, trial_type, sample_size_list) {

  if(hasnt_run(trial_type)){

  # Set seeds
  seed_value <- sample(1:10000, 100, replace=F)
  write.csv(seed_value, paste0(here(),"/OUTPUTS/SEEDS/",trial_type,"_seeds.csv"), row.names = FALSE)
 
  # Set up trial df and set up columns
  subsample_selection_trials <- expand.grid(sampling_size = sample_size_list, seed = seed_value)
  subsample_selection_trials[c(paste0(colnames(training_data),"_p_value"), "duration")] <- 0
  subsample_selection_trials['trial_number'] <- 1:nrow(subsample_selection_trials)
  
  # Run Subsampling trials
  for(row in 1:nrow(subsample_selection_trials)){
    
    print(paste0('Running trial ',row))
    
    # Running subsample size trial
    returned_values <- run_subsample_trial(trial_df = subsample_selection_trials, row_num = row, 
                                                              full_sample = training_data, trial_name = trial_type)
    subsample_selection_trials <- returned_values$trial_df

    # Save time elapsed for the model - Store
    write.csv(subsample_selection_trials, paste0(here(),'/OUTPUTS/TRIAL_INFO/',trial_type,'_trials.csv'), row.names = FALSE)
  
    # Save RData 
    save.image(file = "subsample_selection_trials.RData")
  }
  
  
  # When selecting all candidate subsamples, a mean p-value for each trial is calculated
   subsample_selection_trials <- subsample_selection_trials %>% 
    mutate(mean_p_value = rowMeans(subsample_selection_trials[c('rf_cum_3_day_p_value', 'temperature_p_value', 'ws_p_value', 
                                                                'deg_from_north_p_value', 'dow_p_value', 'working_days_p_value', 
                                                                'hour_p_value', 'pre_peak_hour_p_value', 'pm10_p_value')]))
   
   # These results are saved
  write.csv(subsample_selection_trials, paste0(here(),'/OUTPUTS/TRIAL_INFO/',trial_type,'_trials.csv'), row.names = FALSE)
  

  } else {
    
    print('Already run')
    
  }

  
}
