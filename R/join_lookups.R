join_lookups <- function(data,lookups,join_vars,SHA="main") {
  
  lookuplist <- read_lookups(SHA)
  
  for (i in 1:length(lookups)) {
    
    if (i == 1) {
    
      joined <- dplyr::left_join(data,lookuplist[[lookups[i]]],by=join_vars[i])
      
    } else {
      
      joined <- dplyr::left_join(joined,lookuplist[[lookups[i]]],by=join_vars[i])
      
    }
  }
  
  return(joined)
}