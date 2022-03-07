replace_values <- function(data,changes) {
  
  replaced_data <- data
  
  for (i in 1:length(changes)) {
    
    for (j in 1:nrow(changes[[i]])) {
  
      replaced_data <- dplyr::mutate(replaced_data,
                        "{names(changes[i])}" := replace(!!rlang::sym(names(changes[i])),
                                                  !!rlang::sym(names(changes[i])) == changes[[i]]$old[j],
                                                  changes[[i]]$new[j]))
  
  }
  
    return(replaced_data)
  }
  
}