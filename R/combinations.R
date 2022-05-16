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

combinations <- function(vars,metadata){
  
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

## Summarise indicator variable by a combination list

summarise_by_combination <- function(combination,indicator,root_var,parent_var) {  
  
  df %>% 
    {if(parent != "") dplyr::filter(.,!!sym(root_var) != !!sym(parent_var)) else .} %>%
    dplyr::group_by(dplyr::across(all_of(combination)), .drop=TRUE) %>%
    dplyr::summarise("{indicator}" := sum(.data[[indicator]])) %>%
    dplyr::ungroup()
  
}

