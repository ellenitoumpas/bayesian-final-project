source(here('src', 'models', 'model_string.R'))

#' @title Prepare model
#' @description Creates the model ready to deliver to JAGS
#' @param mu_list
#' @param var_list prior specification
#' @param num_predictions
#' @return
#' @export
#' @examples
prepare_JAGS_model <- function(mu_list, var_list, num_predictions){
  
  modelString <- paste0("
  
  # Scaling data 

  data {
    ysd <- sd(y)
    for ( i in 1:Ntotal ) {
      zy[i] <- y[i] / ysd
    }
    
    for ( j in 1:Nx ) {
      xsd[j] <-   sd(x[,j])
      for ( i in 1:Ntotal ) {
        zx[i,j] <- x[i,j] / xsd[j]
      }
    }
  }

  # Outlining the model
  model{
    for (i in 1:Ntotal) {
      zy[i] ~ dgamma((mu[i]^2)/ zVar, mu[i]/ zVar) # Use sample variance as an estimate of the population variance
      mu[i] <- zbeta0 + sum(zbeta[1:Nx] * zx[i,1:Nx])
    }
    
  zbeta0 ~ dnorm(0, 1/2^2)")
  
  for(i in 1:length(mu_list)){
      modelString <- paste0(modelString, "\n  zbeta[",i,"] ~ dnorm(",mu_list[i],"/xsd[",i,"], 1/(",var_list[i],"/xsd[",i,"]^2))")
  }
  
             
  modelString <- paste0(modelString,
  "\n  zVar ~ dgamma(0.01, 0.001)
  
  # Transform to original scale
  beta[1:Nx] <- (zbeta[1:Nx]/ xsd[1:Nx]) * ysd
  beta0 <- zbeta0 * ysd
  tau <- zVar * (ysd) ^ 2
  
  # Predictions
  ")

  # Write predictions
  for(pred in 1:num_predictions){
    modelString <- paste0(modelString, "\n  pred[",pred,"] <- beta0 ")
    for(mu in 1:length(mu_list)){
      modelString <- paste0(modelString, "+ beta[",mu,"] * xPred[",pred,", ",mu,"] ")
    }
  }
  
  # Finish model string
  modelString <- paste0(modelString,"\n\n}")
  
  # Write to file
  writeLines(modelString, con = "TEMPmodel.txt") 

  
}





#' Title
#' @param codaSamples 
#' @param compVal 
#' @param saveName 
#'
#' @return
#' @export
#'
#' @examples
smryMCMC_HD = function(codaSamples, compVal = NULL,  saveName=NULL) {
  
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  paramName = colnames(mcmcMat)
  
  for ( pName in paramName ) {
    if (pName %in% colnames(compVal)){
      if (!is.na(compVal[pName])) {
        summaryInfo = rbind( summaryInfo , summarizePost( paramSampleVec = mcmcMat[,pName] , compVal = as.numeric(compVal[pName]) ))
      }
      else {
        summaryInfo = rbind( summaryInfo , summarizePost( paramSampleVec = mcmcMat[,pName] ) )
      }
    } else {
      summaryInfo = rbind( summaryInfo , summarizePost( paramSampleVec = mcmcMat[,pName] ) )
    }
  }
  rownames(summaryInfo) = paramName
  
  subDir <- paste0(here(),'/OUTPUTS/SUMMARY/',toupper(saveName),"/")
  if(!file.exists(subDir))(dir.create(file.path(subDir), showWarnings = FALSE))
  
  if (!is.null(saveName))(write.csv(summaryInfo, file=paste0(subDir,saveName,"_SummaryInfo.csv")))
  
  return( summaryInfo )
}

#===============================================================================

#' Title
#'
#' @param codaSamples 
#' @param data 
#' @param xName 
#' @param yName 
#' @param showCurve 
#' @param pairsPlot 
#' @param compVal 
#' @param saveName 
#' @param saveType 
#'
#' @return
#' @export
#'
#' @examples
plotMCMC_HD <- function(codaSamples , data , xName="x" , yName="y" ,
                        showCurve=FALSE ,  pairsPlot=FALSE , compVal = NULL,
                        saveName=NULL , saveType="jpg"){
  y = data[,yName]
  x = as.matrix(data[,xName])
  
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  
  chainLength = NROW( mcmcMat )
  
  zbeta0 = mcmcMat[,"zbeta0"]
  zbeta  = mcmcMat[,grep("^zbeta$|^zbeta\\[",colnames(mcmcMat))]
  
  if (ncol(x)==1) { zbeta = matrix( zbeta, ncol=1) }
  
  zVar = mcmcMat[,"zVar"]
  beta0 = mcmcMat[,"beta0"]
  beta  = mcmcMat[,grep("^beta$|^beta\\[",colnames(mcmcMat))]
  
  if (ncol(x)==1){ beta = matrix( beta, ncol=1)}
  
  tau = mcmcMat[,"tau"]
  
  # Set up predictions
  # number_predictions <- length(grep("^pred", colnames(mcmcMat)))
  number_predictions <- c(1,2,10,20,30,40,50,60,70)
  for (i in number_predictions)(assign(paste0("pred",i), mcmcMat[,paste0("pred[",i,"]")]))
  
  
  #-----------------------------------------------------------------------------
  # Compute R^2 for credible parameters:
  YcorX = cor( y , x ) # correlation of y with each x predictor
  Rsq = zbeta %*% matrix( YcorX , ncol=1 )
  #-----------------------------------------------------------------------------
  
  if ( pairsPlot ) {
    # Plot the parameters pairwise, to see correlations:
    openGraph()
    nPtToPlot = 1000
    plotIdx = floor(seq(1,chainLength,by=chainLength/nPtToPlot))
    panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...) {
      usr = par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y))
      txt = format(c(r, 0.123456789), digits=digits)[1]
      txt = paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex=1.25 ) # was cex=cex.cor*r
    }
    pairs( cbind( beta0 , beta , tau )[plotIdx,] ,
           labels=c( "beta[0]" , 
                     paste0("beta[",1:ncol(beta),"]\n",xName) , 
                     expression(tau) ) , 
           lower.panel=panel.cor , col="skyblue" )
    if ( !is.null(saveName) ) {
      saveGraph( file=paste(saveName,"PostPairs",sep=""), type=saveType)
    }
  }
  
  #-----------------------------------------------------------------------------
  # Marginal histograms:
  
  decideOpenGraph = function(panelCount, saveName, finished=FALSE, nRow=2, nCol=3) {
    # If finishing a set:
    if ( finished==TRUE ) {
      if (!is.null(saveName))(saveGraph(file=paste0(saveName,ceiling((panelCount-1)/(nRow*nCol))), type=saveType))
      panelCount = 1 # re-set panelCount
      return(panelCount)
      
    } else {
      
      # If this is first panel of a graph:
      if (( panelCount %% (nRow*nCol)) == 1 ) {

        # If previous graph was open, save previous one:
        if ( panelCount>1 & !is.null(saveName) )(saveGraph( file=paste0(saveName,(panelCount%/%(nRow*nCol))), type=saveType))
        
        # Open new graph
        openGraph(width=nCol*7.0/3,height=nRow*2.0)
        layout( matrix( 1:(nRow*nCol) , nrow=nRow, byrow=TRUE))
        par(mar=c(4,4,2.5,0.5), mgp=c(2.5,0.7,0))
      }
      # Increment and return panel count:
      panelCount = panelCount+1
      return(panelCount)
    }
  }
  
  subDir <- paste0(here(),'/OUTPUTS/IMAGES/POSTERIOR_DIAGNOSTICS/',toupper(saveName),"/")
  if(!file.exists(subDir))(dir.create(file.path(subDir), showWarnings = FALSE))
  
  
  # Original scale:
  panelCount = 1
  panelCount = decideOpenGraph( panelCount, saveName=paste0(subDir,saveName,"Post_Beta"))
  
  if(!is.na(compVal[["beta0"]])){
    histInfo = plotPost( beta0 , cex.lab = 1.75 , showCurve=showCurve , xlab=bquote(beta[0]) , main="Intercept", compVal = as.numeric(compVal["beta0"] ))
  } else {  
    histInfo = plotPost( beta0 , cex.lab = 1.75 , showCurve=showCurve , xlab=bquote(beta[0]), main="Intercept")
  }
  
  for (bIdx in 1:ncol(beta)) {
    panelCount = decideOpenGraph( panelCount, saveName=paste0(subDir,saveName,"Post_Beta"))
    if (!is.na(compVal[paste0("beta[",bIdx,"]")])) {
      histInfo = plotPost(beta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve , xlab=bquote(beta[.(bIdx)]) , main=xName[bIdx], compVal = as.numeric(compVal[paste0("beta[",bIdx,"]")]))
    } else{
      histInfo = plotPost( beta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve , xlab=bquote(beta[.(bIdx)]) , main=xName[bIdx])
    }
  }
  
  panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(subDir,saveName,"Post_Beta"))

  panelCount = 1
  panelCount = decideOpenGraph(panelCount, saveName=paste0(subDir,saveName,"PostMarg"))
  histInfo = plotPost( tau , cex.lab = 1.75 , showCurve=showCurve , xlab=bquote(tau) , main=paste("Scale") )
  
  panelCount = decideOpenGraph( panelCount, saveName=paste0(subDir,saveName,"PostMarg"))
  histInfo = plotPost( Rsq , cex.lab = 1.75 , showCurve=showCurve , xlab=bquote(R^2) , main=paste("Prop Var Accntd") )
  panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(subDir, saveName,"PostMarg") )
  
  panelCount = 1
  predictions <- c(1,2,10,20,30,40,50,60,70)
  for (i in predictions){
    panelCount = decideOpenGraph( panelCount ,  saveName=paste0(subDir,saveName,"Post_Pred"))
    histInfo = plotPost(get(paste0("pred",i)) , cex.lab = 1.75 , showCurve=showCurve , xlab=paste0("pred",i) , main=paste0("Prediction ",i))
  }
  panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(subDir, saveName,"Post_Pred") )
  
  # Standardized scale:
  panelCount = 1
  panelCount = decideOpenGraph( panelCount , saveName=paste0(subDir,saveName,"PostMargZ") )
  histInfo = plotPost( zbeta0 , cex.lab = 1.75, showCurve=showCurve , xlab=bquote(z*beta[0]) , main="Intercept" )
  
  for ( bIdx in 1:ncol(beta) ) {
    panelCount = decideOpenGraph( panelCount , saveName=paste0(subDir,saveName,"PostMargZ") )
    histInfo = plotPost( zbeta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve, xlab=bquote(z*beta[.(bIdx)]) , main=xName[bIdx] )
  }
  panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(subDir, saveName,"PostMargZ") )
  
  
  
  panelCount = 1
  panelCount = decideOpenGraph( panelCount , saveName=paste0(subDir,saveName,"PostMargZ2"))
  histInfo = plotPost( zVar , cex.lab = 1.75 , showCurve=showCurve , xlab=bquote(z*tau) , main=paste("Scale"))
  
  panelCount = decideOpenGraph( panelCount , saveName=paste0(subDir,saveName,"PostMargZ") )
  histInfo = plotPost( Rsq , cex.lab = 1.75 , showCurve=showCurve , xlab=bquote(R^2) , main=paste("Prop Var Accntd") )
  
  panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(subDir, saveName,"PostMargZ2") )
  
  #-----------------------------------------------------------------------------
}
  
  
#' Title
#'
#' @param parallel 
#' @param data_df 
#' @param model_file 
#' @param recorded_params 
#' @param dependant_var 
#' @param data_list 
#' @param inits_list 
#' @param number_chains 
#' @param number_adaptation_steps 
#' @param burn_in_length 
#' @param chain_iterations 
#' @param thin_steps 
#' @param trial_version 
#' @param summary 
#' @param plot 
#' @param comp_val 
#'
#' @return
#' @export
#'
#' @examples
run_JAGS_model <- function(parallel = FALSE, 
                           data_df = NULL,
                           model_file = "TEMPmodel.txt", 
                           recorded_params = c("mu"), 
                           dependant_var = 'y',
                           data_list = NULL, 
                           inits_list = NULL,
                           number_chains = 2, number_adaptation_steps = 500, 
                           burn_in_length = 500, chain_iterations = 500, 
                           thin_steps = 1, 
                           trial_version = NULL,
                           summary = TRUE , plot = TRUE, 
                           comp_val = compVal){
  
  startTime <- Sys.time()
  
  if(parallel == TRUE){
    
    runJagsOut <- runjags::run.jags(method = "parallel",
                                    model = model_file,
                                    monitor = recorded_params,
                                    data = data_list,
                                    inits = inits_list ,
                                    n.chains = number_chains,
                                    adapt = number_adaptation_steps,
                                    burnin = burn_in_length ,
                                    sample = chain_iterations ,
                                    thin = thin_steps , 
                                    summarise = summary, 
                                    plots = plot)
    
    coda_samples <- coda::as.mcmc.list(runJagsOut)
    
  } else {
    
    jagsModel = runjags::jags.model(file = model_file, 
                                    data = dataList ,      
                                    n.chains = number_chains,
                                    n.adapt = number_adaptation_steps)
    
    update(jagsModel, n.iter = burn_in_length)
    
    coda_samples = coda.samples(jagsModel, variable.names = recorded_params, n.iter = chain_iterations)
    
  }

  subDir <- paste0(here(),'/OUTPUTS/IMAGES/BETA_DIAGNOSTICS/',toupper(trial_version),"/")
  if(!file.exists(subDir))(dir.create(file.path(subDir), showWarnings = FALSE))
  
  # Saving the diagnotic test for each parameter
  for (names in colnames(coda_samples[[1]])){
    diagMCMC(coda_samples , parName = names, saveName = paste0(subDir,trial_version,'_names'))
  }
  
  summary_info <- smryMCMC_HD(codaSamples = coda_samples, compVal = comp_val, saveName = trial_version)
  plotMCMC_HD(codaSamples = coda_samples , data = data_df, xName = colnames(data_list$x), yName = dependant_var, 
              compVal = comp_val, saveName = trial_version )
  
  endTime <- Sys.time()
  time_elapsed <- as.numeric(difftime(endTime, startTime, units='secs'))
  
  return_values <- list(summary_info, time_elapsed)
  names(return_values) <- c('summary_info', 'time_elapsed')
  
  return(return_values)
  
}


#' Title
#'
#' @param lower 
#' @param upper 
#' @param mean 
#' @param sd 
#'
#' @return
#' @export
#'
#' @examples
normal_distribution_plot <- function(lower, upper, mean, sd){
  
  plot_x <- seq(lower, upper, by = .1)
  plot_y <- dnorm(plot_x, mean = mean, sd = sd)
  
  df <- data.frame(x = plot_x, y = plot_y, stringsAsFactors = FALSE)
 
  p <- ggplot2::ggplot(df, aes(x = x, y = y)) + 
    geom_line() + 
    labs(main = paste0("Mean = ",mean,", SD = ",sd), xlab = "", ylab = "") + 
    theme_minimal()
  
  return(p)
}



#' Title
#'
#' @param data 
#' @param n_sample 
#'
#' @return
#' @export
#'
#' @examples
mv_data_subsampling <- function(data, n_sample){
  
  # Desired sample size
  dimensions = names(data)
  generated = head(data,0)
  
  while (nrow(generated) < n_sample) {
    # For debug purposes
    cat(nrow(generated),"\n")
    flush.console()
    
    tmp = data
    
    # Calculate the histogram for each dimension and select one value at a time, slicing the original dataset according to its histogram
    for (i in 1:length(dimensions)) {
      
      colname = dimensions[i]
      
      if (class(data[[colname]]) %in% c("numeric") && 
          sum(data[[colname]] == as.integer(data[[colname]]),na.rm = TRUE) == 0) {
        # Numerical variable. Histogram with Rice's Rule
        # If there are NA's, stratify on those
        
        na_count = sum(is.na(tmp[[colname]]))
        not_na_count = length(tmp[[colname]]) - na_count
        s = sample(c(0,1),prob = c(not_na_count,na_count),1)
        
        if (s == 0) {
          # Histogram stratification based on breaks calculated on the population
          
          n_breaks = floor(2*sum(!is.na(data[[colname]]))**((1/3)))
          bar_size = (max(data[[colname]],na.rm = TRUE)-min(data[[colname]],na.rm = TRUE))/n_breaks
          breaks = sapply(0:n_breaks,function(i) {min(data[[colname]],na.rm = TRUE) + i*bar_size})
          
          h = hist(tmp[[colname]],breaks=breaks,plot = F)
          
          # Select one bar of the histogram according to the density
          bar_id  = sample(1:length(h$mids),prob = h$counts,1)
          
          bar_start = h$breaks[bar_id]
          bar_end = h$breaks[bar_id + 1]
          
          tmp = tmp[tmp[[colname]] >= bar_start & tmp[[colname]] < bar_end & !is.na(tmp[[colname]]),]
        } else {
          # NA
          tmp = tmp[is.na(tmp[[colname]]),]
        }
        
      } else {
        
        # Categorical variable
        # Histogram for the selected dimension
        aggr = as.data.frame(table(tmp[,colname],useNA="ifany"))
        names(aggr) = c("dim","count")
        
        # Generate a value according to the histogram
        generated_value = sample(aggr$dim,prob=aggr$count,1)
        
        # Slice the actual multivariate histogram in order to take only records with the selected value on the selected dimension
        if (!is.na(generated_value))(tmp = tmp[tmp[[colname]] == generated_value & !is.na(tmp[[colname]]),])else(tmp = tmp[is.na(tmp[[colname]]),])
        
      }
      
    }
    
    # Once the procedure finishes, we get a bulk of records with the same values of each dimension. Let's take one of these records uniformly
    random_index = sample(1:nrow(tmp),1)
    new_record = tmp[random_index,]
    
    # Let's remove duplicates
    inserted_record = sqldf::sqldf("select * from new_record except select * from generated")
    
    # Insert in the "generated" data frame and repeat until desired sample size is reached
    generated = rbind(generated,inserted_record)
    
  }
  
  return(generated)
  
}



#' @title View model summary table
#' @description After a model is run the summary is stored in a text file. With this script we can view the trial's summary. 
#' @param trial_name the name of the trial
#' @return data.frame
#' @export
#' @examples
view_summary_table <- function(trial_name){
  
  file_path <- paste0(here(),'/OUTPUTS/SUMMARY/',toupper(trial_name),"/") 
  
  summary <- read.csv(paste0(file_path,trial_name,"_SummaryInfo.csv"), stringsAsFactors = FALSE)
  summary <- summary %>% 
    select(-c('CompVal', 'PcntGtCompVal', 'ROPElow', 'ROPEhigh', 'PcntLtROPE', 'PcntInROPE', 'PcntGtROPE'))
  
  return(summary)
  
}



#' Title
#'
#' @param trial_name 
#'
#' @return
#' @export
#'
#' @examples
view_trial_info <- function(trial_name){
  
  file_path <- paste0(here(),'/OUTPUTS/TRIAL_INFO/') 
  trial_info <- read.csv(paste0(file_path,trial_name,"_details.csv"), stringsAsFactors = FALSE)
  
  return(trial_info)
  
}





#' Title
#'
#' @param trial_name 
#' @return
#' @export
#' @examples
view_regression_diagnostics_images <- function(trial_name){
  file_path <- paste0(here(),'/OUTPUTS/IMAGES/BETA_DIAGNOSTICS/',toupper(trial_name),"/")
  knitr::include_graphics(c(paste0(file_path,trial_name,'_namesDiagbeta0.jpg'), 
                            paste0(file_path,trial_name,'_namesDiagbeta[1].jpg'), 
                            paste0(file_path,trial_name,'_namesDiagbeta[2].jpg'), 
                            paste0(file_path,trial_name,'_namesDiagbeta[3].jpg'), 
                            paste0(file_path,trial_name,'_namesDiagbeta[4].jpg'), 
                            paste0(file_path,trial_name,'_namesDiagbeta[5].jpg'),
                            paste0(file_path,trial_name,'_namesDiagbeta[6].jpg'),
                            paste0(file_path,trial_name,'_namesDiagbeta[7].jpg'),
                            paste0(file_path,trial_name,'_namesDiagbeta[8].jpg'),
                            paste0(file_path,trial_name,'_namesDiagtau.jpg')))
  }


#' @title
#' @description 
#' @param trial_name 
#' @return
#' @export
#' @examples
view_prediction_diagnostics_images <- function(trial_name){
  file_path <- paste0(here(),'/OUTPUTS/IMAGES/BETA_DIAGNOSTICS/',toupper(trial_name),"/")
  knitr::include_graphics(c(paste0(file_path,trial_name,'_namesDiagpred[1].jpg'), 
                            paste0(file_path,trial_name,'_namesDiagpred[2].jpg'), 
                            paste0(file_path,trial_name,'_namesDiagpred[10].jpg'), 
                            paste0(file_path,trial_name,'_namesDiagpred[20].jpg'), 
                            paste0(file_path,trial_name,'_namesDiagpred[30].jpg'), 
                            paste0(file_path,trial_name,'_namesDiagpred[40].jpg'), 
                            paste0(file_path,trial_name,'_namesDiagpred[50].jpg'),
                            paste0(file_path,trial_name,'_namesDiagpred[60].jpg'),
                            paste0(file_path,trial_name,'_namesDiagpred[70].jpg')))
}



#' @title
#' @description 
#' @param trial_name 
#' @return
#' @export
#' @examples
view_posterior_images <- function(trial_name){
  
  file_path <- paste0(here(),'/OUTPUTS/IMAGES/POSTERIOR_DIAGNOSTICS/',toupper(trial_name),"/")
  knitr::include_graphics(c(paste0(file_path,trial_name,'Post_Beta1.jpg'),
                            paste0(file_path,trial_name,'Post_Beta2.jpg'),
                            paste0(file_path,trial_name,'Post_Pred1.jpg'),
                            paste0(file_path,trial_name,'Post_Pred2.jpg'),
                            paste0(file_path,trial_name,'PostMarg1.jpg'),
                            paste0(file_path,trial_name,'PostMargZ1.jpg'),
                            paste0(file_path,trial_name,'PostMargZ2.jpg'),
                            paste0(file_path,trial_name,'PostMargZ21.jpg')))
}



