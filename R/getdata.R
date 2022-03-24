## Function to read dataset
## Takes information from setsource.R to locate the initial data
## Only imports variables specified in in_vars argument

getdata <- function(date,datasource,in_vars) {
    
    # Read dataset (function taken from setsource.R)
    
    raw_data <- readsource(date,datasource)
    
    # If in_vars not supplied then include all variables from original dataset
    # Otherwise only include those specified in in_vars 
    
    if (is.null(in_vars)) {in_vars <- names(raw_data)}
    
    # Add time variables (function taken from addtime.R)
    
    timeinfo <- addtime(raw_data,date,datasource,in_vars)
    
    # Reduce dataset to specified variables
    
    raw_data <- timeinfo$raw_data %>% dplyr::select(dplyr::any_of(timeinfo$in_vars))
        
    return(raw_data)
    
  }