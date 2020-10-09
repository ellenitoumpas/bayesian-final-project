library('here')
source(here('src', 'model', 'model_fine_tune.R'))
source(here('src', 'model', 'model_math.R'))
source(here('src', 'model', 'model_posterior.R'))
source(here('src', 'model', 'model_prior.R'))
source(here('src', 'model', 'model_string.R'))
source(here('constants.R'))


#' @title Get Initial Values
#' @description For the initial values 
#' @param df the training data dataframe
#' @param method a string determing which method to use
#' @return list
#' @export
#' @examples
get_initial_values <- function(df, method = "likelihood-mean", intercept = 0.1, variance = 0.01, pred = NULL){
  

  if(!is.null(pred)){
    
    isMethod = !missing(method)
    
    if(method == "likelihood-mean" || !isMethod) {
      
      if(!isMethod) { print('Method not specified, using likelihood-mean') }
      
      if(method == "likelihood-mean"){
        
        # Initial value of intercept
        initial_values_list <- c(intercept)
      
        # Initial value for each independant variable regression parameter
        for (col in colnames(df)){
          if(col != pred)(initial_values_list <- c(initial_values_list, (mean(df[[col]])/sd(df[[col]]))))
        }
      
        # Initial value of variance
        initial_values_list <- c(initial_values_list, variance)
      
      }

    } else {
      print("You need to supply the prediction variable.")    
  }
  
  # If there is a hypothesis for another method of calculating initial values add it as a possible here with a 'method' so we can call it
  
  return(initial_values_list)
  
}


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

#' @title Model adapt steps
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

#' @title Model burn in steps
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
