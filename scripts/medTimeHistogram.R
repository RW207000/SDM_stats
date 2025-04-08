medTimePlot <- function(therapy_data, medicine, ttl){
  
  medData <- therapy_data[therapy_data$Medicine_1.x == medicine,]
  
  if (nrow(medData) == 0) {
    print("no medicines with that name")
    return()
  }
  
  timings_long_set_up <- medData %>%
    pivot_longer(cols = c(dose_time_hours_1,
                          dose_time_hours_2,
                          dose_time_hours_3,
                          dose_time_hours_4),
                 names_to = c("dose_number"),
                 values_to = "time")
  
  timings_long <- timings_long_set_up[, c("dose_number", "time", "Medicine_1.x", "Medicine_2.x", "Medicine_3.x")]
  
  
  dosePlot <- timings_long %>%
    ggplot(aes(x = time, fill = dose_number)) +
    geom_histogram() +
    theme_minimal() +
    scale_fill_hue(labels = c("dose 1", "dose 2", "dose 3")) +
    labs(title = ttl, x = "time")
  
  return(dosePlot)
  
  
}