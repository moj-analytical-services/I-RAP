join_lookups <- function(data,lookups,join_vars,SHA="main") {
  
  lookuplist <- read_lookups(SHA)
  
  join_one <- function(lookups,join_vars,data,lookuplist) {
    
    add_vars <- setdiff(names(lookuplist[[lookups]]),join_vars)
    
    dup_vars <- intersect(add_vars,names(data))
    
    joined <- dplyr::left_join(data,lookuplist[[lookups]],by=join_vars,suffix=c(".OLD",".NEW"))
    
    remove_duplicates <- function(dup_vars,joined) {
      
      dedup <- joined %>% dplyr::mutate("{dup_vars}" := do.call(dplyr::coalesce,dplyr::across(c(paste0(dup_vars,c(".NEW",".OLD"))))),
                                        .keep = "unused") %>%
        dplyr::select(!ends_with(c(".OLD",".NEW")))
      
      return(dedup)
      
    }
    
    if (length(dup_vars) > 0) {
      
      joined <- lapply(dup_vars,FUN=remove_duplicates,joined=joined) %>%
        purrr::reduce(merge)
      
    }
    
    joined <- joined %>% dplyr::select(c("case_ref",add_vars,join_vars))
    
    return(joined)
    
  }
  
  
  data_refs <- dplyr::mutate(data,case_ref = row_number())
  
  joined_all <- mapply(join_one,lookups,join_vars,MoreArgs = list(data=data_refs,lookuplist=lookuplist), SIMPLIFY = TRUE) %>%
    purrr::reduce(dplyr::left_join, by="case_ref") %>%
    dplyr::left_join(data_refs,by="case_ref",suffix=c("",".y")) %>%
    dplyr::select(!dplyr::ends_with(".y"),-"case_ref")
  
}