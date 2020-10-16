
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
model_name <- 'model_037'

# Set Hyper Params here:
nChains <- number_chains <- 3
adaptSteps <- number_adaptation_steps <- 500 # Number of steps to "tune" the samplers
burnInSteps <- burn_in_steps <- 500
thinningSteps <- thinning_steps <- 3

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
parameters = c( "zbeta0" ,  "zbeta" , "beta0" ,  "beta" ,  "tau", "zVar") # Here beta is a vector!

days_of_weeks <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
wind_directions <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW','NNW')

seed_value <- sample(1:10000, 20, replace=F)
