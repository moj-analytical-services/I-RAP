readsource <- function(date,datasource) {
  
  # Import source data as specfied in setsource.R
  
  source <- setsource(date,datasource)
  
  # Import data using correct function
  
  if (source$format == "SAS") {
    
    data <- botor::s3_read(paste0("s3://",source$s3path),
                           haven::read_sas)
  } 
  
  # Run any extra processing specified in setsource.R
  
  if (!is.na(source$extra_processing)) {
    
    data <- get(source$extra_processing)(data)
    
  }
  
  return(data)
  
}
