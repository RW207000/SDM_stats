summariseMed <- function(minimalDataset, medicine, dose_type) {

  medData <- minimalDataset[minimalDataset$Medicine_1.x == medicine,]
  
  if (nrow(medData) == 0) {
    print("no medicines with that name")
    return()
  }
  
  if (dose_type == "raw") {
  
  StatSummary <- medData %>%
    group_by(Centre.ID.x) %>%
    summarise("mean" = mean(Total_raw_dose, na.rm = T),
              "median" = median(Total_raw_dose, na.rm = T),
              "SD" = sd(Total_raw_dose, na.rm = T),
              "min" = min(Total_raw_dose, na.rm = T),
              "max" = max(Total_raw_dose, na.rm = T),
              "n" = length(Total_raw_dose[!is.na(Total_raw_dose)])) %>%
    mutate_if(is.numeric, ~round(., 2))
  }
  
  else if (dose_type == "standardised") {
    StatSummary <- medData %>%
      group_by(Centre.ID.x) %>%
      summarise("mean" = mean(total_standardised_daily_dose_1_1, na.rm = T),
                "median" = median(total_standardised_daily_dose_1_1, na.rm = T),
                "SD" = sd(total_standardised_daily_dose_1_1, na.rm = T),
                "min" = min(total_standardised_daily_dose_1_1, na.rm = T),
                "max" = max(total_standardised_daily_dose_1_1, na.rm = T),
                "n" = length(total_standardised_daily_dose_1_1[!is.na(total_standardised_daily_dose_1_1)])) %>%
      mutate_if(is.numeric, ~round(., 2)) 
    
  }
    
  else {
    print("incorrect dose type")
  }
  
  return (StatSummary)
  }
