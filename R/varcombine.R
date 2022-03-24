varcombine <- function(data,newvar,startvars,FUN,na.val = NA) {
  
  if (FUN == "mean") {
    combineFUN <- function(x){rowMeans(x,na.rm=TRUE)}
  } else if (FUN == "sum") {
    combineFUN <- function(x){rowSums(x,na.rm=TRUE)}
  }
  
  data <- data %>%
    dplyr::mutate("{newvar}" := dplyr::select(., startvars) %>% combineFUN())
  
  data[[newvar]][is.na(data[[newvar]])] <- na.val
  
  return(data)
  
}