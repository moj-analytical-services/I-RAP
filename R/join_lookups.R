join_lookups <- function(data,lookups,join_vars,SHA="main") {
  
  lookuplist <- read_lookups(SHA)
  
  for (i in 1:length(lookups)) {
    
    add_vars <- names(lookuplist[[lookups[i]]])[!names(lookuplist[[lookups[i]]]) %in% join_vars]
    
    if (i == 1) {
    
      joined <- dplyr::left_join(data,lookuplist[[lookups[i]]],by=join_vars[i],suffix=c(".OLD",".NEW"))
      
      for (j in 1:length(add_vars)) {
      
        if (add_vars[j] %in% names(data)) {
          
          joined <- joined %>% mutate("{add_vars[j]}" := do.call(coalesce,across(c(paste0(add_vars[j],c(".NEW",".OLD"))))),
                                    .keep = "unused")
        }
      
      }
      
    } else {
      
      original <- joined
      
      joined <- dplyr::left_join(joined,lookuplist[[lookups[i]]],by=join_vars[i],suffix=c(".OLD",".NEW"))
      
      for (j in 1:length(add_vars)) {
        
        if (add_vars[j] %in% names(original)) {
          
          joined <- joined %>% mutate("{add_vars[j]}" := do.call(coalesce,across(c(paste0(add_vars[j],c(".NEW",".OLD"))))),
                                    .keep = "unused")
        }
        
      }
      
    }
  }
  
  return(joined)
}