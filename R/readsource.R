readsource <- function(date,datasource) {
  
  # Import source data as specfied in setsource.R
  
  source <- setsource(date,datasource)
  
  # Import data using correct function
  
  if (source$format == "SAS") {
    
    data <- paws_read_using(FUN = haven::read_sas,
                            path = source$s3path)
  } 
  
  # Run any extra processing specified in setsource.R
  
  if (!is.na(source$extra_processing)) {
    
    data <- get(source$extra_processing)(data)
    
  }
  
  return(data)
  
}
