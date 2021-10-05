prison_pop_datasource <- function(date) {
  
  s3path <- stringr::str_interp("alpha-dag-omsq/population/ao${date}.sas7bdat")
  
  population_data <- paws_read_using(FUN = haven::read_sas,
                                     path = s3path)
  
  return(population_data)
  
}

prison_receptions_datasource <- function(date) {
  
  s3path <- stringr::str_interp("alpha-dag-omsq/receptions/receps${date}.sas7bdat")
  
  receptions_data <- paws_read_using(FUN = haven::read_sas,
                                     path = s3path)
  
  receptions_data$Subsequent_JISLlen_Band[receptions_data$Untried_Flag == 1] <- "Remand untried"
  receptions_data$Subsequent_JISLlen_Band[receptions_data$Convicted_Flag == 1] <- "Remand convicted"
  receptions_data$Subsequent_JISLlen_Band[receptions_data$Civil_Flag == 1] <- "Civil"
  receptions_data$Subsequent_JISLlen_Band[receptions_data$Subsequent_JISLlen_Band == "L Unknown length – determinate sentence" ] <- "Unknown"
  
  receptions_data <- receptions_data %>% 
                        dplyr::mutate(age = dplyr::select(., Untried_Age, Sentenced_Age, Recall_Age, Convicted_Age, First_Age) %>% rowMeans(na.rm = TRUE))
  
  receptions_data$age[is.na(receptions_data$age)] <- 0

  return(receptions_data)
  
}