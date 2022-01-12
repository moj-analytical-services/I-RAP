## Function to add time variables to dataset
## Returns a list consisting of the updated dataset and a vector of time variables

addtime <- function(data,date,datasource) {
  
  # Update retained variable list to accept time variables
  
  time_vars <- c("time_period","time_identifier")
  
  # Add relevant time variables for each data source

  if (datasource == "prison_pop") {
    
    data$time_period <- stringr::str_sub(date,1,4)
    data$time_identifier <- months(as.Date(date,format="%Y%m%d"))
    
  } else if (datasource %in% c("prison_receptions","prison_releases")) {
    
    data$time_period <- stringr::str_sub(date,1,4)
    data$time_identifier <- "Calendar year"
    data$quarter <- stringr::str_to_upper(stringr::str_sub(date,5,6))
    
    time_vars <- c(time_vars,"quarter")
    
  }

return(list(data=data,time_vars=time_vars))
  
}