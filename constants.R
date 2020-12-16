
# MODEL
# # Area: Every m2 increase in land size increases the sales price by 90 AUD. This is a very strong expert knowledge.
# zbeta[1] ~ dnorm( 90/100000/xsd[1] , 0.01/(4/xsd[1]^2) ) # 1/ variance for normal distribution

# # Bedrooms: Every additional bedroom increases the sales price by 100,000AUD. This is a weak expert knowledge.
# zbeta[2] ~ dnorm( 1/xsd[3] , 1/(1/xsd[3]^2) ) # 1/ variance for normal distribution

# # Bathrooms: There is no expert knowledge on the number of bathrooms.
# zbeta[3] ~ dnorm( 0 , 1/4 ) # 1/ variance for normal distribution

# # CarParks: Every additional car space increases the sales price by 120,000AUD. This is a strong expert knowledge.
# zbeta[4] ~ dnorm( 120/xsd[4] , 1/(0.1/xsd[4]^2) ) # 1/ variance for normal distribution

# # PropertyType: If the property is a unit, the sale price will be 150,000 AUD less than that of a house on the average. This is a very strong expert knowledge.
# zbeta[5] ~ dnorm( 150/xsd[4] , 1/(0.01/xsd[4]^2) ) # 1/ variance for normal distribution

# Set Priors here:
mus <- c(-1, # rain
         1, # temp
         # 1, # ws
         -1, # deg_from_north
         # 1, # dow
         1, # working_days
         # 0, # hour
         1) #pre_peak


vars  <- c(100, # rain
           100, # temp
           # 1/1000, # ws
           100, # deg_from_north
           # 1/1000, # dow
           # 1/10, # working_days informative
           100, # working_days uninformative
           # 1/10, # hour
           100) #pre_peak

# Set Hyper Params here:
# TODO: Do we need both of these?
model_name <- 'model_100_phil_intercept'
# model_049_gamma_gamma_c3_a500_b500_t5

# Set Hyper Params here:
nChains <- number_chains <- 3
adaptSteps <- number_adaptation_steps <- 500 # Number of steps to "tune" the samplers
burnInSteps <- burn_in_steps <- 500
thinningSteps <- thinning_steps <- 5

# FIXME: move to constants
# JAGS time! # remove hour
features <- c(
  'rf_cum_3_day',
  'temperature',
  # 'ws',
  'deg_from_north',
  # 'dow',
  'working_days',
  # 'hour',
  'pre_peak_hour',
  'pm10'
)

predictor <- 'pm10'


parameters <- c("beta0", "beta", "zbeta0", "zbeta", "tau", "zVar", "pred") # new
parameters <- c( "zbeta0" ,  "zbeta" , "beta0" ,  "beta" ,  "tau", "zVar") # Here beta is a vector!

days_of_weeks <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
wind_directions <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW','NNW')

seed_value <- sample(1:10000, 20, replace=FALSE)

# Zero intercept?:
zero_intercept = FALSE

# Create Init values list:
# initial_values <- get_initial_values(subsample_data, method = "likelihood-mean", pred = "pm10")
# output: 
# [1] 0.1000000 0.4941515 2.6739331 1.4063048 1.5836989 0.5933827 0.0100000
# initial_values <- c(0.1000000, 0.4941515, 2.6739331, 1.4063048, 1.5836989, 0.5933827, 0.0100000) # based on likelihood-mean
# initial_values <- c(1/2, 1/2, 1/1000, 1/2, 1/1000, 1/10, 1/10) # Optimal Vars 1  inf5d had the best diags (but shocking predictions)
initial_values <- c(10, 10, 1/100, 10, 1/100, 1/10, 1/10) # Optimal Vars 2 inf5e had better predictions but not as good diags
