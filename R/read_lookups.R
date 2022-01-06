read_lookups <- function(SHA="main") {
  
  req <- httr::GET(paste0("https://api.github.com/repos/moj-analytical-services/harmonisation-reference/git/trees/",SHA,"?recursive=1"))
  
  httr::stop_for_status(req)
  
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  
  filelist <- filelist[stringr::str_detect(filelist,"reference_files/[a-z_]+.csv")]
  
  lookups <- lapply(filelist,FUN=function(x){read.csv(paste0("https://raw.github.com/moj-analytical-services/harmonisation-reference/",SHA,"/",x))})
  
  lookups <- lapply(lookups,FUN=function(x){dplyr::mutate(x,dplyr::across(dplyr::everything(), as.character))})

  names(lookups) <- stringr::str_sub(basename(filelist),1,-5)
  
  return(lookups)
}