
inspect_data <- function(data) {

  data %>% select(id, rainfall_mm, rf_cum_2_day, rf_cum_3_day, rf_cum_4_day, rf_cum_5_day) %>% 
    head(5) %>%
    format_table(p_caption = "Inspecting the first six rows of the data.", text_size = 6)  %>%
    kableExtra::landscape() %>%
    kableExtra::row_spec(0, angle = 90)
    
  data %>% select(rf_cum_6_day, rf_cum_7_day, date_day, date_local_time, site) %>% 
    head(5) %>%
    format_table(p_caption = "Inspecting the first six rows of the data.", text_size = 6)  %>%
    kableExtra::landscape() %>%
    kableExtra::row_spec(0, angle = 90)
  
  data %>% select(temperature, pm10, pm10a, wd, ws, dow, hour, winddire) %>%
    head(5) %>%
    format_table(p_caption = "Inspecting the first six rows of the data.", text_size = 6)  %>%
    kableExtra::landscape() %>%
    kableExtra::row_spec(0, angle = 90)
  
  data %>% select(years, yn80, roll_temp, yn50, north, north1, yn60, weekdays, mornings) %>%
    head(5) %>%
    format_table(p_caption = "Inspecting the first six rows of the data.", text_size = 6)  %>%
    kableExtra::landscape() %>%
    kableExtra::row_spec(0, angle = 90)
}
