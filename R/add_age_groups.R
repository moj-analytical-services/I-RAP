# Function to add age groupings to dataset from harmonised lookups

add_age_groups <- function(data,age_lookups,SHA="main") {
  
  lookups <- read_lookups(SHA)
  
  if (age_lookups == "all") {
    
    lookups <- lookups[grep("age",names(lookups))]  
    
  } else {
    
    lookups <- lookups[age_lookups]
    
  }

  data$age <- as.numeric(data$age)
  
  for (i in 1:length(lookups)) {
    
    data[[names(lookups)[i]]] <- cut(data$age,breaks=c(lookups[[i]]$start_age,999),
                                                labels=lookups[[i]]$label,
                                                right=FALSE)
    
  }
  
  return(data)

}