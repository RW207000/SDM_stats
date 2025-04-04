summariseStat <- function(statName, minimalDataset) {
  
  colNames <- names(minimalDataset)
  nameIdx <- colNames == statName
  
  if (sum(nameIdx) == 0) {
    print(sum(nameIdx))
    print("no variable with that name")
    return (FALSE)
  }
  

  
  StatSummary <- minimalDataset %>%
    group_by(Centre.ID.x) %>%
    summarise("mean" = mean(minimalDataset, na.rm = T),
              "median" = median(minimalDataset, na.rm = T),
              "SD" = sd(minimalDataset, na.rm = T),
              "min" = min(minimalDataset, na.rm = T),
              "max" = max(minimalDataset, na.rm = T),
              "n" = length(minimalDataset[!is.na(minimalDataset)])) %>%
    mutate_if(is.numeric, ~round(., 2))
  
  ## change shape of data frame
  # statSummaryFrame <- data.frame(cbind(names(StatSummary), t(StatSummary)))
  # statSummaryFrame <- select(statSummaryFrame, -X1)
  # colnames(statSummaryFrame) <- c(statName)
  
  return (StatSummary)
  
}