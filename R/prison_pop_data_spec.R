prison_pop_data_spec <- function(date) {
  
  s3path <- stringr::str_interp("alpha-dag-omsq/population/ao${date}.sas7bdat")
  
  population_data <- paws_read_using(FUN = haven::read_sas,
                                     path = s3path)
  
  return(population_data)
  
}