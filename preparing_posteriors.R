
mus <- c(-1, # rain
         1, # temp
         # 1, # ws
         -1, # deg_from_north
         # 1, # dow
         9, # working_days
         # 0, # hour
         10) #pre_peak


vars  <- c(10, # rain
           10, # temp
           # 1/1000, # ws
           10, # deg_from_north
           # 1/1000, # dow
           1/10, # working_days informative
           10, # working_days uninformative
           # 1/10, # hour
           1/10) #pre_peak



# Options for 
normal_distribution_plot(8, 10, 9, 1/10)
normal_distribution_plot(8, 10, 9, 1)
normal_distribution_plot(8, 10, 9, 10)
