
multi_row_margin <- theme(plot.margin =  unit(c(0.5,0.5,0.5,0.5), "cm"))

assignment_plot_theme <-
  theme(axis.text = element_text(size=8), 
        axis.title = element_text(size=8, margin = margin(t = 20, r = 20, b = 20, l = 20)))


format_table <- function(pre_pipeline, p_caption=NULL, width = TRUE){
  pre_pipeline %>% knitr::kable(caption = p_caption) %>% kableExtra::kable_styling(full_width = width)
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
