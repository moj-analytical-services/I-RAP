prison_receptions_extra <- function(data) {
  
  receptions_data <- data

  receptions_data <- receptions_data %>% 
    dplyr::mutate(age = dplyr::select(., Untried_Age, Sentenced_Age, Recall_Age, Convicted_Age, First_Age) %>% rowMeans(na.rm = TRUE))
  
  receptions_data$age[is.na(receptions_data$age)] <- 0
  
  return(receptions_data)

}