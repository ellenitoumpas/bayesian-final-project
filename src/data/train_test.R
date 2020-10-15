train_split <- function(df, features, target) {
  features_ <- if(hasArg(target)) { features } else { features[-length(features)] }
  
  df <- df %<>%
    mutate(pm10 = if_else(pm10 < 0, 0, pm10)) %>%
    filter(!(date_day %in% c(ymd('2018-12-28'), ymd('2018-12-29'), ymd('2018-12-30'), ymd('2018-12-31')))) %>%
    select_(.dots = features_) 
  
  if('dow' %in% colnames(df))(df <- df %>% mutate(dow = as.numeric(dow)))
  if('working_days' %in% colnames(df))(df <- df %>% mutate(working_days = as.numeric(working_days)))
  if('pre_peak_hour' %in% colnames(df))(df <- df %>% mutate(pre_peak_hour = as.numeric(pre_peak_hour)))
    
  return(df)
}

test_split <- function(df, features, target) {
  
  features_ <- if(hasArg(target)) { features } else { features[-length(features)] }
  
  df <- df %<>%
    mutate(pm10 = if_else(pm10 < 0, 0, pm10)) %>%
    filter((date_day %in% c(ymd('2018-12-28'), ymd('2018-12-29'), ymd('2018-12-30'), ymd('2018-12-31')))) %>%
    select_(.dots = features_)
    
  if('dow' %in% colnames(df))(df <- df %>% mutate(dow = as.numeric(dow)))
  if('working_days' %in% colnames(df))(df <- df %>% mutate(working_days = as.numeric(working_days)))
  if('pre_peak_hour' %in% colnames(df))(df <- df %>% mutate(pre_peak_hour = as.numeric(pre_peak_hour)))
    
    
  return(df)
}

