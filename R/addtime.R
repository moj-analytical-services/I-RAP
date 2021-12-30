addtime <- function(raw_data,date,datasource,in_vars) {
  
  # Update retained variable list to accept time variables
  
  in_vars <- c(in_vars,"time_period","time_identifier")
  
  # Add relevant time variables for each data source

  if (datasource == "prison_pop") {
    
    raw_data$time_period <- stringr::str_sub(date,1,4)
    raw_data$time_identifier <- months(as.Date(date,format="%Y%m%d"))
    
  } else if (datasource == "prison_receptions") {
    
    raw_data$time_period <- stringr::str_sub(date,1,4)
    raw_data$time_identifier <- "Calendar year"
    raw_data$quarter <- stringr::str_to_upper(stringr::str_sub(date,5,6))
    
    in_vars <- c(in_vars,"quarter")
    
  }

return(list(raw_data=raw_data,in_vars=in_vars))
  
}