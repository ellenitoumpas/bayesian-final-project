
model_trial_type <- "model_001_gamma_gamma_c3_a500_b500_t3"

number_chains = 3
number_adaptation_steps = 500
burn_in_steps = 500
thinning_steps = 3

adaptSteps = 1500  # Number of steps to "tune" the samplers
burnInSteps = 12000

# Mean specification
mus <- c(-40, 40, 0, -40, 0, 20, 0, 20)

# Variance specification
vars  <- c(1/4, 1/2, 1/10, 1/2, 1/10, 1/2, 1/10, 1/2)

parameters = c( "zbeta0" ,  "zbeta" , "beta0" ,  "beta" ,  "tau", "zVar") # Here beta is a vector!

days_of_weeks <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
wind_directions <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW','NNW')
