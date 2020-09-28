
#' @title Univariate plots
#' @description An exploration in to the distribution of univariate features
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
#' @description Pulls the relevant statistics for that column
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



multi_row_margin <- theme(plot.margin =  unit(c(0.5,0.5,0.5,0.5), "cm"))

assignment_plot_theme <-
  theme_minimal() +
  theme(axis.text = element_text(size=8), 
        axis.title = element_text(size=8, margin = margin(t = 20, r = 20, b = 20, l = 20)))


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
