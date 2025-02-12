library('ggplot2')

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



clean_existing_data <- function(){
  
  data <- vroom::vroom(here('data', 'hour_extra_Brooklyn_2016_18.csv'))
  data_cleaned <- data %>% janitor::clean_names()
  
  data_cleaned <- data_cleaned %>%
    mutate(dow = factor(dow, levels = days_of_weeks, labels = days_of_weeks, ordered = TRUE),
           winddire = factor(winddire, levels = wind_directions, labels = wind_directions, ordered = TRUE))
  
  data_cleaned <- data_cleaned %>% select(-id, -yn50, -yn60, -yn80, -pm10a)
  data_cleaned <- data_cleaned[complete.cases(data_cleaned),]
  holidays <- tsibble::holiday_aus(unique(data_cleaned$years), state = 'VIC')
  
  data_cleaned <- data_cleaned %>%
    mutate(deg_from_north = case_when(wd > 180 ~ abs(wd - 360), TRUE ~ wd),
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
}





prediction_density_overlay <- function(summaryInfo_df, x_data, y_data, trial_type, sample_type){
  
  # Get the model coefficients out
  coefficients <- summaryInfo_df[rownames(summaryInfo_df)[grepl("^beta", rownames(summaryInfo_df))],"Mode"]
  
  # Get the variance out
  Variance <- summaryInfo_df["tau","Mode"]
  
  # Use the model (X*beta) to generate the mean of gamma population for each observed x vector.
  # meanGamma <- as.matrix(cbind(rep(1,nrow(x_data)),  x_data)) %*% as.vector(coefficients)
  meanGamma <- as.matrix(cbind(x_data)) %*% as.vector(coefficients)
  
  # Generate random data from the posterior distribution.
  randomData <- rgamma(n= 231,shape=meanGamma^2/Variance, rate = meanGamma/Variance)
  
  # Display the density plot of observed data and posterior distribution:
  predicted <- data.frame(elapsed = randomData)
  observed <- data.frame(elapsed = y_data)
  predicted$type <- "Predicted"
  observed$type <- "Observed"
  dataPred <- rbind(predicted, observed)
  
  p <- ggplot(dataPred, aes(elapsed, fill = type)) +
    geom_density(alpha = 0.2) +
    theme_minimal()
  
  ggsave(glue::glue('OUTPUTS/IMAGES/PREDICTION_DENSITY/{trial_type}_{sample_type}.png'), p, width = 5, height = 4, dpi = 200)
  
  return(p)
  
}




predictions_visual <- function(summaryInfo_df, trial_type){
  
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
  
  ggsave(glue::glue('OUTPUTS/IMAGES/PREDICTIONS/{trial_type}.png'), p, width = 5, height = 4, dpi = 200)
  
  p
  
}








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
setup_run_JAGS_trial <- function(data, 
                                 predictor, 
                                 predictions, 
                                 mu_list, 
                                 var_list, 
                                 initial_values, 
                                 params, 
                                 par_trial_name, 
                                 num_predictions,
                                 zero_intercept){

  # Setting up and saving trial data frame info
  trial_info <- as.data.frame(matrix(ncol = 0, nrow = 1))
  
  trial_info$trial_name <- par_trial_name
  
  for(i in 1:length(mu_list))(trial_info[[paste0('mu',stringr::str_pad(as.character(i), width = 2, side = "left", pad = "0"))]] <- mu_list[i])
  for(i in 1:length(var_list))(trial_info[[paste0('var',stringr::str_pad(as.character(i), width = 2, side = "left", pad = "0"))]] <- var_list[i])
  
  trial_info$number_chains <- params$number_chains
  trial_info$number_adaptation_steps <- params$number_adaptation_steps
  trial_info$burn_in_steps <- params$burn_in_steps
  trial_info$thinning_steps <- params$thinning_steps
  
  for(i in 1:length(initial_values))(trial_info[[paste0('initial_values_list',stringr::str_pad(as.character(i), width = 2, side = "left", pad = "0"))]] <- initial_values[i])
  
  trial_info$duration = 0

  
  # Split independant and dependant variables
  y_data <- data[[predictor]]
  x_data <- as.matrix(data[,!(colnames(data) %in% predictor)])
  
  # Specify data list for JAGS
  dataList <- list(
    x = x_data ,
    y = y_data ,
    xPred = predictions,
    Nx = dim(x_data)[2] ,
    Ntotal = dim(x_data)[1]
  )
  
  # Prepare JAGS model
  prepare_JAGS_model(mu_list = mu_list,
                     var_list = var_list,
                     num_predictions = num_predictions,
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
                                    trial_version = par_trial_name)
  
  # Save time elapsed for the model - Store
  trial_info[1,'duration'] <- returned_values['time_elapsed']
  write.csv(trial_info, paste0(here(),'/OUTPUTS/TRIAL_INFO/',
                               par_trial_name,
                               '_details.csv'), row.names = FALSE)
  
  # Save RData 
  save.image(file = paste0(here(),'/OUTPUTS/RData/',par_trial_name,".RData"))
  
  return(returned_values)
  
}











#' Title
#'
#' @param pre_pipeline 
#' @param p_caption 
#' @param width 
#' @param text_size 
#'
#' @return
#' @export
#'
#' @examples
format_table <- function(pre_pipeline, p_caption=NULL, width = TRUE, text_size = NULL){
  pre_pipeline %>% knitr::kable(caption = p_caption) %>% kableExtra::kable_styling(full_width = width, font_size = text_size)
}





#' Title
#'
#' @param time 
#'
#' @return
#' @export
#'
#' @examples
get_time <- function(time) {
  time %>%
    stringr::str_split(" ") %>%
    purrr::map_chr(2) %>%
    hms()
}





#' Title
#'
#' @param values 
#' @param isID 
#'
#' @return
#' @export
#'
#' @examples
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





#' Title
#'
#' @param data 
#' @param uniqueIdentifier 
#'
#' @return
#' @export
#'
#' @examples
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
