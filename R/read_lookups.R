read_lookups <- function(SHA="main") {
  
  req <- httr::GET(paste0("https://api.github.com/repos/moj-analytical-services/harmonisation-reference/git/trees/",SHA,"?recursive=1"))
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  
  filelist <- filelist[stringr::str_detect(filelist,"reference_files/[a-z_]+.csv")]
  
  lookups <- lapply(filelist[-stringr::str_which(filelist,"age")],FUN=function(x){read.csv(paste0("https://raw.github.com/moj-analytical-services/harmonisation-reference/",SHA,"/",x))})
  
  agespecs <- lapply(filelist[stringr::str_which(filelist,"age")],FUN=function(x){read.csv(paste0("https://raw.github.com/moj-analytical-services/harmonisation-reference/",SHA,"/",x))})
  
  lookups <- lapply(lookups,FUN=function(x){dplyr::mutate(x,dplyr::across(dplyr::everything(), as.character))})

  agespecs <- lapply(agespecs,FUN=function(x){dplyr::mutate(x,dplyr::across(dplyr::everything(), as.character))})
  
  names(agespecs) <- stringr::str_sub(basename(filelist[stringr::str_which(filelist,"age")]),1,-5)
  
  lookups[[length(lookups)+1]] <- agespecs
  
  names(lookups) <- c(stringr::str_sub(basename(filelist[-stringr::str_which(filelist,"age")]),1,-5),"agespecs")
  
  return(lookups)
}