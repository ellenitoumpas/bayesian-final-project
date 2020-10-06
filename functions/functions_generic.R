
# GGPLOT theme for plot consistency
assignment_plot_theme <- theme_minimal() +
  theme(title = element_text(size = 8, colour = '#333333'),
        axis.text = element_text(size = 8, colour = '#333333'), 
        axis.title = element_text(size = 8, margin = margin(t = 20, r = 20, b = 20, l = 20), colour = '#333333'))


# GGPLOT theme for multiple row plot exports
assignment_multi_plot_theme <- theme_minimal() + 
  theme(plot.margin =  unit(c(0.5,0.5,0.5,0.5), "cm"), 
        title = element_text(size = 6, colour = '#333333'),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 7, colour = '#333333'), 
        axis.title = element_text(size = 7, margin = margin(t = 20, r = 20, b = 20, l = 20), colour = '#333333'))




#' @title Univariate plots
#' @description An exploration in to the distribution of univariate features. A different plot type based on the class of variable.
#' @param values univariate values to plot
#' @param name the name of the column being plotted
#' @return ggplot plot
#' @export
#' @examples
univariate_distribution_plot <- function(values, name){
  
  ######### SPECIFY DEFAULTS #########
  
  # If categorical and more than 20 values
  # This is a trigger to display only top 20
  
  top_100 <- FALSE
  df <- data.frame(value = values, stringsAsFactors = TRUE)
  
  ######### FOR FACTORS #########
  
  if(class(values)[1] %in% c("ordered","factor")){
    
    frequency <- df %>% group_by(value) %>% summarise(count = n()) %>% arrange(desc(count))
    
    # If nrows > 100 cut down to head
    if(nrow(frequency) > 100){
      frequency <- frequency %>% head(100)
      top_20 <- TRUE
    }
    
    p <- ggplot(frequency, aes(x = reorder(value, desc(count)), y = count)) +
      geom_bar(stat="identity", fill = "#98c1d9") +
      theme_minimal() +
      labs(x = name,
           y = "count",
           title = if(top_100==TRUE)(paste0("Frequency of top 100 values from ",name," column"))else(paste0("Frequency of ",name," column")))
    
    if(nrow(frequency) > 5)(p <- p + theme(axis.text.x = element_text(angle = 90)))
    
    
    ######### FOR NUMERIC/ DATE #########
    
  } else if(class(values)[1] %in% c("numeric", "Date", "integer")){
    
    p <- ggplot(df, aes(x = value)) +
      geom_histogram(fill = "#98c1d9") +
      theme_minimal() +
      labs(x = name,
           y = "count",
           title = paste0("Distribution of ",name," column"))
    
  } else if(class(values)[1] %in% c("times")){
    
    df$value <- as.POSIXct(strptime(df$value, format="%H:%M:%S"))
    
    p <- ggplot(df, aes(x = value)) +
      geom_histogram(fill = "#98c1d9") +
      theme_minimal() +
      labs(x = name,
           y = "count",
           title = paste0("Distribution of ",name," column")) +
      scale_x_datetime(breaks = scales::date_breaks("2 hour"), labels = date_format("%H:%M"))
    
    p <- p + theme(axis.text.x = element_text(angle = 90))
    
  } else {
    
    print(paste0(name," couldn't be plotted."))
    p <- ""
    
  }
  
  p <- p + 
    theme(plot.title = element_text(size = 10,face="bold", margin=margin(20,0,10,0)), 
          axis.title=element_text(size=10))
  
  return(p)
  
}





#' @title Pull statistics for the single column
#' @description Pulls the relevant statistics for that column dependant on class type.
#' @param values values to calculate summary of
#' @param isID if this variable is an ID ignores mode
#' @return list
#' @export
#' @examples
exploratory_summary <- function(values, name = NULL, isID = FALSE){
  
  type <- class(values %>% select(!!rlang::sym(name)) %>% pull())
  
  missing_values <- values %>% select(!!rlang::sym(name)) %>% pull() %>% is.na() %>% sum()
  mean <- if(type %in% c('numeric', 'integer'))(round(mean(values %>% select(!!rlang::sym(name)) %>% pull(), na.rm = TRUE),3))else(NA)
  median <- if(type %in% c('numeric', 'integer'))(round(median(values %>% select(!!rlang::sym(name)) %>% pull(), na.rm = TRUE),3))else(NA)
  sd <- if(type %in% c('numeric', 'integer'))(round(sd(values %>% select(!!rlang::sym(name)) %>% pull(), na.rm = TRUE),3))else(NA)
  
  if((type %in% c('factor','Date')) & (isID == FALSE)){
    
    mode <- values %>% 
      select(!!rlang::sym(name)) %>% 
      group_by(!!rlang::sym(name)) %>% 
      summarize(count = n()) %>% 
      arrange(desc(count)) %>% 
      top_n(1) %>% 
      select(!!rlang::sym(name)) %>% 
      pull() %>% 
      as.character() %>% 
      paste0(collapse = ",")
    
    mode_count <- values %>% 
      filter(!!rlang::sym(name) == mode) %>% 
      nrow()
    
  } else {
    mode <- NA
    mode_count <- NA
  }
  
  min <- if(type %in% c('numeric', 'integer', 'Date'))(min(values %>% select(!!rlang::sym(name)) %>% pull(), na.rm = TRUE))else(NA)
  max <- if(type %in% c('numeric', 'integer', 'Date'))(max(values %>% select(!!rlang::sym(name)) %>% pull(), na.rm = TRUE))else(NA)
  nlevs <- if((type == 'factor'))(values %>% select(!!rlang::sym(name)) %>% pull() %>% unique() %>% length())else(NA)
  
  return_values <- list(name, type, missing_values, mean, median, sd, mode, mode_count, min, max, nlevs)
  names(return_values) <- c('name', 'type', 'missing_values', 'mean', 'median', 'sd', 'mode', 'mode_count', 'min', 'max', 'nlevs')
  return(return_values)
  
}





#' @title Collate summary table
#' @description Collates the summary table for the data based on the class type
#' @param data data to create summary of
#' @param uniqueIdentifier unique identifier to ignore in mode calculation
#' @return data.frame
#' @export
#' @examples
exploratory_summarize <- function(data, uniqueIdentifier = NULL){
  
  summary_df <- data.frame(name = character(), 
                           type = character(), 
                           missing_values = character(), 
                           mean = character(),
                           median = character(), 
                           sd = character(), 
                           mode = character(), 
                           mode_count = character(),
                           min = character(),
                           max = character(),
                           nlevs = character(), 
                           stringsAsFactors = FALSE)
  
  for(col in colnames(data)){
    
    if(col == uniqueIdentifier)(summary <- exploratory_summary(data, name = col, isID = TRUE))else(summary <- exploratory_summary(data, name = col))
    
    summary_df <- summary_df %>% 
      add_row(name = summary[['name']] %>% as.character(), 
              type = summary[['type']] %>% as.character(), 
              missing_values = summary[['missing_values']] %>% as.character(), 
              mean = summary[['mean']] %>% as.character(),
              median = summary[['median']] %>% as.character(), 
              sd = summary[['sd']] %>% as.character(), 
              mode = summary[['mode']] %>% as.character(), 
              mode_count = summary[['mode_count']] %>% as.character(),
              min = summary[['min']] %>% as.character(),
              max = summary[['max']] %>% as.character(),
              nlevs = summary[['nlevs']] %>% as.character())
  }
  
  return(summary_df)
  
}





#' @title Hasn't run
#' @description Checks if this test has run before by seeing if a trial information document exists in the OUTPUTS/TRIAL_INFO/ folder. 
#' If the file exists then the function returns false. This function can be used in an IF ELSE statement to prevent models running when
#' they have fun before.
#' @param test the trial name (what the trial is testing)
#' @return boolean
#' @export
#' @examples 
hasnt_run <- function(test){
  !(list.files(path = paste0(here(),"/OUTPUTS/TRIAL_INFO/")) %>% 
      grep(pattern = test) %>% 
      any())
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





#' Title
#'
#' @param data 
#' @param predictor 
#' @param predictions 
#' @param mu_list 
#' @param var_list 
#' @param initial_values 
#' @param params 
#'
#' @return
#' @export
#'
#' @examples
run_subsample_size_JAGS_trial <- function(data, predictor, predictions, mu_list, var_list, initial_values, params, trial_num){
  
  # Split independant and dependant variables
  y_data <- data[[predictor]]
  x_data <- as.matrix(data[,!(colnames(data) %in% predictor)])
  
  # Specify data list for JAGS
  dataList <- list(
    x = x_data ,
    y = y_data ,
    # xPred = predictions,
    Nx = dim(x_data)[2] ,
    Ntotal = dim(x_data)[1]
  )
  
  # Prepare JAGS model
  prepare_JAGS_model(beta1_mu = mu_list[1], beta1_var = var_list[1],
                     beta2_mu = mu_list[2], beta2_var = var_list[2],
                     beta3_mu = mu_list[3], beta3_var = var_list[3],
                     beta4_mu = mu_list[4], beta4_var = var_list[4],
                     beta5_mu = mu_list[5], beta5_var = var_list[5],
                     beta6_mu = mu_list[6], beta6_var = var_list[6],
                     beta7_mu = mu_list[7], beta7_var = var_list[7],
                     beta8_mu = mu_list[8], beta8_var = var_list[8])
  
  # Set up monitoring parameters
  # parameters <- c("beta0", "beta", "zbeta0", "zbeta", "tau", "zVar", "pred")
  parameters <- c("beta0", "beta", "zbeta0", "zbeta", "tau", "zVar")
  
  # Set up BLANK comparison values
  compVal <- data.frame("beta0" = NA, "beta[1]" = NA, "beta[2]" = NA,  "beta[3]" = NA, "beta[4]" =  NA,  "beta[5]" =  NA, 
                        "beta[6]" =  NA, "beta[7]" =  NA, "beta[8]" =  NA, "tau" = NA , check.names = FALSE)
  
  # Set initial values
  if(!is.null(initial_values)){
    initsList <- list(
      zbeta0 = initial_values[1],
      zbeta = initial_values[seq(from = 2, to = length(initial_values)-1, by = 1)],
      zVar = initial_values[length(initial_values)]
    )
    
    print(initsList)
    
  } else {
    initsList <- NULL
  }
  
  
  # Run the JAGS model
  returned_values <- run_JAGS_model(parallel = TRUE, 
                                    model_file = "TEMPmodel.txt",  
                                    dependant_var = predictor,
                                    data_df = data,
                                    recorded_params = parameters,  
                                    data_list = dataList,
                                    inits_list = initsList,  
                                    comp_val = compVal,  
                                    number_chains = params$number_chains,
                                    number_adaptation_steps = params$number_adaptation_steps,
                                    burn_in_length = params$burn_in_steps, 
                                    chain_iterations = ceiling((params$burn_in_steps * params$thinning_steps)/ params$number_chains),
                                    thin_steps = params$thinning_steps, 
                                    summary = FALSE, 
                                    plot = FALSE,
                                    trial_version = paste0("Trial_",stringr::str_pad(trial_num, width = 3, side = "left", pad = "0")))
  
  return(returned_values)
  
}











format_table <- function(pre_pipeline, p_caption=NULL, width = TRUE, text_size = NULL){
  pre_pipeline %>% knitr::kable(caption = p_caption) %>% kableExtra::kable_styling(full_width = width, font_size = text_size)
}





get_time <- function(time) {
  time %>%
    stringr::str_split(" ") %>%
    purrr::map_chr(2) %>%
    hms()
}





exploratory_summary <- function(values, isID = FALSE){
  
  name <- colnames(values)
  type <- class(values %>% pull())
  missing_values <- values %>% pull() %>% is.na() %>% sum()
  mean <- if(type == 'numeric')(round(mean(values %>% pull(), na.rm = TRUE),3))else(NA)
  median <- if(type == 'numeric')(round(median(values %>% pull(), na.rm = TRUE),3))else(NA)
  sd <- if(type == 'numeric')(round(sd(values %>% pull(), na.rm = TRUE),3))else(NA)
  
  if((type %in% c('factor','Date')) & (isID == FALSE)){
    mode <- data[,name] %>% 
      group_by(!!rlang::sym(name)) %>% 
      summarize(count = n()) %>% 
      arrange(desc(count)) %>% 
      top_n(1) %>% 
      select(name) %>% 
      pull() %>% 
      as.character() %>% 
      paste0(collapse = ",")
  } else {
    mode <- NA
  }
  
  min <- if(type %in% c('numeric', 'Date'))(min(values %>% pull(), na.rm = TRUE))else(NA)
  max <- if(type %in% c('numeric', 'Date'))(max(values %>% pull(), na.rm = TRUE))else(NA)
  nlevs <- if((type == 'factor'))(values %>% pull() %>% unique() %>% length())else(NA)
  
  return_values <- list(name, type, missing_values, mean, median, sd, mode, min, max, nlevs)
  names(return_values) <- c('name', 'type', 'missing_values', 'mean', 'median', 'sd', 'mode', 'min', 'max', 'nlevs')
  
  return(return_values)
}





exploratory_summarize <- function(data, uniqueIdentifier = NULL){
  
  summary_df <- data.frame(name = character(), 
                           type = character(), 
                           missing_values = character(), 
                           mean = character(),
                           median = character(), 
                           sd = character(), 
                           mode = character(), 
                           min = character(),
                           max = character(),
                           nlevs = character(), 
                           stringsAsFactors = FALSE)
  
  for(col in colnames(data)){
    
    if(col == uniqueIdentifier)(summary <- exploratory_summary(data[,col],isID = TRUE))else(summary <- exploratory_summary(data[,col]))
    
    summary_df <- summary_df %>% 
      add_row(name = summary[['name']] %>% as.character(), 
              type = summary[['type']] %>% as.character(), 
              missing_values = summary[['missing_values']] %>% as.character(), 
              mean = summary[['mean']] %>% as.character(),
              median = summary[['median']] %>% as.character(), 
              sd = summary[['sd']] %>% as.character(), 
              mode = summary[['mode']] %>% as.character(), 
              min = summary[['min']] %>% as.character(),
              max = summary[['max']] %>% as.character(),
              nlevs = summary[['nlevs']] %>% as.character())
  }
  
  return(summary_df)
  
}





# Calculate distance in kilometers between two points
# https://conservationecology.wordpress.com/2013/06/30/distance-between-two-points-in-r/
earth.dist <- function (long1, lat1, long2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}


# Reference packages we might use
# askpass, bookdown, base64enc, curl, docstring, dplyr, ggplot2, here, htmltools, httr, janitor, jsonlite, kableExtra, lubridate, openssl, purrr, rmarkdown, rvest, selectr, snakecase, sys, testthat, tidyr, tinytex, vroom, webshot, xml2, tidygeocoder, geosphere, tidyverse, backport, gridExtra, GGally, caret, 
# install unloaded packages then library the lot
lock_n_load_libraries <- function(required_packages) {
  
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])] 
  
  
  if (length(missing_packages)) {
      install.packages(missing_packages)
  }

  print(sapply(required_packages, require, character.only = TRUE))

  # take a renv snapshot:
  library('renv')
  renv::snapshot()
  # don't forget to commit this!

}

