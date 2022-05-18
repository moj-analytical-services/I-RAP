tabulate <- function(data,vars,metadata,indicator) {
  
  combinations <- list_combinations(vars,metadata)
  
  root_var <- lapply(combinations,find_root_var,metadata=metadata)
  
  parent_var <- lapply(root_var,function(var){sapply(var,find_parent,metadata=metadata)})
  
  table <- purrr::pmap(list(root_var,parent_var,combinations),summarise_by_combination,data=data,indicator=indicator) %>%
    bind_rows
  
  return(table)
  
}