library('dplyr')
library('here')

impossible_values <- function(data) {

data.frame(column = character(), nrows = integer()) %>%

  add_row(column = 'rainfall_mm', nrows = data_cleaned %>%
  	filter(rainfall_mm < 0) %>%
  	nrow()) %>%

  add_row(column = 'rf_cum_2_day', nrows = data_cleaned %>%
  	filter(rf_cum_2_day < 0) %>%
  	nrow()) %>%

  add_row(column = 'rf_cum_3_day', nrows = data_cleaned %>%
  	filter(rf_cum_3_day < 0) %>%
  	nrow()) %>%

  add_row(column = 'rf_cum_4_day', nrows = data_cleaned %>%
  	filter(rf_cum_4_day < 0) %>%
  	nrow()) %>%

  add_row(column = 'rf_cum_5_day', nrows = data_cleaned %>%
  	filter(rf_cum_5_day < 0) %>%
  	nrow()) %>%

  add_row(column = 'rf_cum_6_day', nrows = data_cleaned %>%
  	filter(rf_cum_6_day < 0) %>%
  	nrow()) %>%

  add_row(column = 'rf_cum_7_day', nrows = data_cleaned %>%
  	filter(rf_cum_7_day < 0) %>%
  	nrow()) %>%

  add_row(column = 'rf_cum_5_day', nrows = data_cleaned %>%
  	filter(rf_cum_5_day < 0) %>%
  	nrow()) %>%

  add_row(column = 'date_day', nrows = data_cleaned %>%
  	filter(date_day < dmy('01/01/2016')) %>%
	rbind(data_cleaned %>%
	filter(date_day > dmy('31/12/2018'))) %>%
	nrow()) %>%
				
  add_row(column = 'temperature', nrows = data_cleaned %>%
  	filter(temperature < 0) %>%
	rbind(data_cleaned %>%
	filter(temperature > 50)) %>%
	nrow()) %>%
				
  add_row(column = 'pm10', nrows = data_cleaned %>%
  	filter(pm10 < 0) %>%
  	nrow()) %>%

  add_row(column = 'pm10a', nrows = data_cleaned %>%
  	filter(pm10a < 0) %>%
  	nrow()) %>%

  add_row(column = 'wd', nrows = data_cleaned %>%
  	filter(wd < 0) %>%
	rbind(data_cleaned %>%
	filter(wd > 360)) %>%
	nrow()) %>%
				
  add_row(column = 'ws', nrows = data_cleaned %>%
  	filter(ws < 0) %>%
  	nrow()) %>%

  add_row(column = 'hour', nrows = data_cleaned %>%
  	filter(hour < 0) %>%
		rbind(data_cleaned %>%
		filter(hour > 23)) %>%
		nrow()) %>%
				
  add_row(column = 'years', nrows = data_cleaned %>%
  	filter(years < 2016) %>%
	rbind(data_cleaned %>%
	filter(years > 2018)) %>%
	nrow()) %>%
				
  add_row(column = 'roll_temp', nrows = data_cleaned %>%
  	filter(roll_temp < 0) %>%
	rbind(data_cleaned %>%
	filter(roll_temp > 50)) %>%
	nrow()) %>%
				
  format_table(
	p_caption = "The number of rows with impossible values."
  )

}