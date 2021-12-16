readsource <- function(date,datasource) {
  
  source <- setsource(date,datasource)
  
  if (source$format == "SAS") {
    
    data <- paws_read_using(FUN = haven::read_sas,
                            path = source$s3path)
  } 
  
  if (!is.na(source$extra_processing)) {
    
    data <- get(source$extra_processing)(data)
    
  }
  
  return(data)
  
}
