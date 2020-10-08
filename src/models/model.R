#' @title Math model
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

model_math <- function(df){

  # TODO: make the function do something. It currently returns the original dataframe
  return(df)
  
}

#' @title Get Initial Values
#' @description
#' @param
#' @param
#' @return
#' @export
#' @examples

get_initial_values <- function(df){
  # Select initial values based on candidate subsample
  sd_rf_cum_3_day <- sd(df$rf_cum_3_day)
  sd_temperature <- sd(df$temperature)
  sd_ws <- sd(df$ws)
  sd_deg_from_north <- sd(df$deg_from_north)
  sd_dow <- sd(df$dow)
  sd_working_days <- sd(df$working_days)
  sd_hour <- sd(df$hour)
  sd_pre_peak_hour <- sd(df$pre_peak_hour)
  sd_pm10 <- sd(df$pm10) # CHECKME: sd_pm10 isn't used anywhere

  initial_values_list <- c(0.1,
    mean(df$rf_cum_3_day) / sd_rf_cum_3_day,
    mean(df$temperature) / sd_temperature,
    mean(df$ws) / sd_ws,
    mean(df$deg_from_north) / sd_deg_from_north,
    mean(df$dow) / sd_dow,
    mean(df$working_days) / sd_working_days,
    mean(df$hour) / sd_hour,
    mean(df$pre_peak_hour) / sd_pre_peak_hour,
    0.01
  )
  
  return(initial_values_list)
}

model

#' @title Prior specification
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

model_prior <- function(df){

  # TODO: make the function do something. It currently returns the original dataframe
  return(df)
  
}

#' @title Prior specification
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

model_posterior <- function(df){

  # TODO: make the function do something. It currently returns the original dataframe
  return(df)
  
}


#' @title Model efficiency
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

model_efficiency <- function(df){

  # TODO: make the function do something. It currently returns the original dataframe
  return(df)
  
}

#' @title Model efficiency
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

model_adapt_steps <- function(df){

  # TODO: make the function do something. It currently returns the original dataframe
  return(df)
  
}

#' @title Model burn_in steps
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

model_burn_in_steps <- function(df){

  # TODO: make the function do something. It currently returns the original dataframe
  return(df)
  
}

#' @title Model thinning steps
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

model_thinning_steps <- function(df){

  # TODO: make the function do something. It currently returns the original dataframe
  return(df)
  
}

#' @title Model saved steps
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

model_saved_steps <- function(df){

  # TODO: make the function do something. It currently returns the original dataframe
  return(df)
  
}

#' @title Model initial values
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

model_initial_values <- function(df){

  # TODO: make the function do something. It currently returns the original dataframe
  return(df)
  
}

#' @title Model fine tuning
#' @description 
#' @param 
#' @param 
#' @return 
#' @export
#' @examples

model_fine_tuning <- function(df){

  # TODO: make the function do something. It currently returns the original dataframe
  return(df)
  
}

