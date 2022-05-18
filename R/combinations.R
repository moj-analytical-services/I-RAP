## Functions for creating combinations from a list of variables
## Used to generate totals for tabulations

## Create all possible combinations

all_combinations <- function(vars) {
  
  combos <- 1:length(vars) %>% 
    purrr::map(
      ~
        gtools::combinations(length(vars),.x,vars)
    ) %>% 
    lapply(FUN=function(x) split(x,1:nrow(x))) %>%
    do.call(c,.)
  
}

## Filter to only those combinations that are valid.
## A combination is invalid if it contains a variable but not its parent variable.

list_combinations <- function(vars,metadata){
  
  # Filter metadata to only those variables where the parent is in the variable list
  
  meta_parent <- dplyr::filter(metadata, parent %in% vars)
  
  # Identify any combinations that contain an orphan variable with no parent
  
  orphans <-purrr::map(all_combinations(vars),
                function(meta){ 
                  purrr::map2(meta_parent$variable,meta_parent$parent,
                       function(variable,parent){
                         variable %in% meta && !parent %in% meta
                         }
                       )
                  }
                )
  
  # Retain only those that do not contain an orphan
  
  valid_combinations <- all_combinations(vars)[!sapply(orphans,any)]
  
  return(valid_combinations)
}

filterforcombo <- function(data,root_var,parent_var) {
  
  for (i in 1:length(root_var)) {
    
    if (i==1) {
      
      df <- data
    }
    
    df <- dplyr::filter(df,!!sym(root_var[i]) != !!sym(parent_var[i]))
    
  }
  
  return(df)
  
}



summarise_by_combination <- function(data,root_var,parent_var,combination,indicator) {  
  
  data %>% 
    {if(length(root_var) > 0) filterforcombo(.,root_var,parent_var) else .}  %>%
    dplyr::group_by(dplyr::across(all_of(combination)), .drop=TRUE) %>%
    dplyr::summarise("{indicator}" := sum(.data[[indicator]])) %>%
    dplyr::ungroup()
  
}

## Find variables from vector that are lowest in a heirarchy

find_root_var <- function(vars,metadata){
  
  meta_parent <- filter(metadata,parent != "" | parent %in% vars) %>%
    filter(variable %in% vars)
  
  return(vars[sapply(vars,function(x){x %in% meta_parent$variable && !x %in% meta_parent$parent})])
  
}

find_parent <- function(var,metadata) {
  
  if (length(var) > 0) { 
    
    dplyr::filter(metadata,variable==var) %>% 
      dplyr::pull(parent)
    
  }
  
}

