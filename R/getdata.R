getdata <- function(date,datasource,in_vars) {
    
    # Read datasets from raw files as specified in setsource.R
    
    raw_data <- readsource(date,datasource)
    
    # If in_vars not supplied then include all variables from original dataset
    
    if (is.null(in_vars)) {in_vars <- names(raw_data)}
    
    # Add time variables
    
    timeinfo <- addtime(raw_data,date,datasource,in_vars)
    
    # Reduce dataset to specified variables
    
    raw_data <- timeinfo$raw_data %>% dplyr::select(dplyr::any_of(timeinfo$in_vars))
        
    return(raw_data)
    
  }