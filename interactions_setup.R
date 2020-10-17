# interaction set up

# for the vars:
# rain, temp, deg-north, pre-peak

# with signif interactions:
# rain:deg, rain:pre-peak, deg:pre-eak

subsample_data <- subsample_data %>% 
  mutate(
    rain_deg = rf_cum_3_day * deg_from_north,
    rain_pre_peak = rf_cum_3_day * pre_peak_hour,
    deg_pre_peak = deg_from_north * pre_peak_hour
  )

mus <- c(-2, # rain
         1.2, # temp
         # 1, # ws
         -1, # deg_from_north
         # 1, # dow
         # 9, # working_days
         # 0, # hour
         15, #pre_peak
         0.1, # rain:deg
         -0.4,# rain:pre_peak
         -0.1# deg:pre_peak
         ) 


vars  <- c(100, # rain
           1/1000, # temp
           # 1/100, # ws
           10, # deg_from_north
           # 1/100, # dow
           # 10, # working_days uninformative
           # 1/10, # hour
           1/100, #pre_peak
           10, # rain:deg
           10, # rain:pre_peak
           1/10 # deg:pre_peak
           ) 

