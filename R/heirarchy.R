## Find variables from vector that are lowest in a heirarchy

root_var <- function(vars,metadata){
  
  meta_parent <- filter(metadata,parent != "" | parent %in% vars) %>%
                  filter(variable %in% vars)
  
  return(vars[sapply(vars,function(x){x %in% meta_parent$variable && !x %in% meta_parent$parent})])
  
}