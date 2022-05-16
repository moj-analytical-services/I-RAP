
metadata <- tribble(~"variable", ~"parent",
                "custody_type", "custody_group",
                "custody_detail", "custody_type",
                "custody_group","",
                "sex","",
                "prison_type","sex")

vars <- c("sex","custody_group","custody_type","custody_detail")


combos <- combinations(vars,metadata)

bottom <- root_var(vars,metadata)

parent_var <- dplyr::filter(metadata,variable==bottom) %>%
                dplyr::pull(parent)

  c2 <- combos
has_parent <- TRUE

i <- 1

while(parent_var != ""){
  
  parent <- dplyr::filter(metadata,variable==bottom) %>%
    pull(parent)
  
  has_parent <- parent != ""
  
  c3 <- if(has_parent) {
    c2[sapply(c2,function(x){bottom %in% x})]
  } else {c2}
  
  new_table <- lapply(c3,gsum,indicator="prisoners",bottom=bottom,parent=parent,has_parent=has_parent) %>%
    dplyr::bind_rows()
  
  if (i == 1){
    
    part_table <- new_table
    
  } else {
    
    part_table <- bind_rows(part_table,new_table)
    
  }
  
  
  if (has_parent){
  
  vars <- vars[vars != bottom]
  
  c2 <- c2[!sapply(c2,function(x){bottom %in% x})]
  
  bottom <- parent
  
  i <- i+1
  
  }

  
}



  

