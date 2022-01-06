join_lookups <- function(data,lookups,custom_lookups,join_vars,SHA="main") {
  
  if (!is.null(lookups)) {
  
    lookuplist <- read_lookups(SHA)
  
    lookuplist <- lookuplist[lookups]

  }
  
  if (!is.null(custom_lookups)) {
    
    if (is.null(lookups)) {
      
      lookuplist <- custom_lookups
      
    } else {
      
      lookuplist <- c(custom_lookups,lookuplist)
      
    }
    
  }
  
  default_vars <- lapply(lookuplist,function(x){names(x)[1]})
  
  add_default <- function(x,y){if(is.null(x)){x<-y} else x<-x}
  
  if (!is.null(join_vars)) {
    
    join_vars <- mapply(add_default,join_vars,default_vars)
    
  } else {
    
    join_vars <- default_vars
    
  }
  
  join_one <- function(lookup,join_vars,data) {
    
    add_vars <- setdiff(names(lookup),join_vars)
    
    dup_vars <- intersect(add_vars,names(data))
    
    joined <- dplyr::left_join(data,lookup,by=join_vars,suffix=c(".OLD",".NEW"))
    
    if (length(dup_vars) > 0) {
      
      joined <- joined %>% 
        dplyr::mutate("{dup_vars}" := do.call(dplyr::coalesce,
                                              dplyr::across(c(paste0(dup_vars,c(".NEW",".OLD")))))) %>%
        dplyr::select(-paste0(dup_vars,c(".NEW",".OLD")))
      
    }
    
    joined <- dplyr::select(joined,-join_vars)
    
    return(joined)
    
  }
  
  joined <- data
  
  for (i in 1:length(lookuplist)) {
    
    joined <- join_one(lookuplist[[i]],join_vars[[i]],joined)
    
  }
    
  return(joined)
    
}