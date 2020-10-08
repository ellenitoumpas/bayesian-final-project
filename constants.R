adaptSteps = 1500  # Number of steps to "tune" the samplers
burnInSteps = 12000

nChains = 3 
thinSteps = 31 # First run for 3
numSavedSteps = 4000

parameters = c( "zbeta0" ,  "zbeta" , "beta0" ,  "beta" ,  "tau", "zVar") # Here beta is a vector!


days_of_weeks <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
wind_directions <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW','NNW')
