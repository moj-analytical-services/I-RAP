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
  
  names(receptions_data) <- tolower(names(receptions_data))
  
  receptions_data$subsequent_jisllen_band <- sub(".*? (.+)", "\\1", receptions_data$subsequent_jisllen_band)
  
  receptions_data <- receptions_data %>% dplyr::filter(first_flag == 1)  
  
  receptions_data$subsequent_jisllen_band[(receptions_data$untried_flag == 1 | receptions_data$convicted_flag == 1)] <- "Remand"
  receptions_data$subsequent_jisllen_band[receptions_data$civil_flag == 1] <- "Civil"
  
  return(receptions_data)
  
}