readsource <- function(date,datasource,in_vars) {
  
  # Import data specifications from setsource.R
  
  source <- setsource(date,datasource)
  
  # Import data using correct function (see readformat.R)
  
  data <- readformat(source$s3path, source$format)
  
  # Run any extra processing specified in setsource.R
  
  if (!is.na(source$extra_processing)) {
    
    data <- get(source$extra_processing)(data)
    
  }
  
  # Add time variables
  
  data_time <- addtime(data,date,datasource)
  
  # If in_vars not supplied then include all variables from original dataset
  
  if (is.null(in_vars)) {in_vars <- names(data)}
  
  # Limit dataset to only in_vars and time variables
  
  final_data <- data_time$data %>% dplyr::select(dplyr::any_of(c(in_vars,data_time$time_vars))) 
  
  return(final_data)
  
}
