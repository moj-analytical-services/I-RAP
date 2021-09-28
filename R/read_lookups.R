read_lookups <- function(SHA="main") {
  
  req <- GET(paste0("https://api.github.com/repos/moj-analytical-services/harmonisation-reference/git/trees/",SHA,"?recursive=1"))
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  
  filelist <- filelist[stringr::str_detect(filelist,"reference_files/[a-z_]+.csv")]
  
  lookups <- lapply(filelist[-stringr::str_which(filelist,"age")],FUN=function(x){read.csv(paste0("https://raw.github.com/moj-analytical-services/harmonisation-reference/",SHA,"/",x))})
  
  agespec <- lapply(filelist[stringr::str_which(filelist,"age")],FUN=function(x){read.csv(paste0("https://raw.github.com/moj-analytical-services/harmonisation-reference/",SHA,"/",x))})
  
  names(agespec) <- str_sub(basename(filelist[stringr::str_which(filelist,"age")]),1,-5)
  
  lookups[[length(lookups)+1]] <- agespec
  
  names(lookups) <- c(str_sub(basename(filelist[-stringr::str_which(filelist,"age")]),1,-5),"agespec")
  
  return(lookups)
}