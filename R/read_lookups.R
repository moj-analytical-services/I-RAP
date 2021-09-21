read_lookups <- function() {
  
  lookups <- jsonlite::fromJSON("https://raw.github.com/moj-analytical-services/harmonisation-reference/main/reference_files/lookups.json")
  lookups <- lapply(lookups,FUN=function(x){dplyr::mutate(x,dplyr::across(dplyr::everything(), as.character))})
  lookups <- lapply(lookups,FUN=function(x){x[is.na(x)] <- ""
                                              return(x)})
  
  lookups$agespecs <- jsonlite::fromJSON("https://raw.github.com/moj-analytical-services/harmonisation-reference/main/reference_files/agespecs.json")
  
  return(lookups)
}
  
