
metadata <- tribble(~"variable", ~"parent",
                "custody_type", "custody_group",
                "custody_detail", "custody_type",
                "custody_group","",
                "sex","",
                "prison_type","sex")

vars <- c("sex","prison_type")


combos <- combinations(vars,metadata)


is_bottom <- function(x){
  
  meta_parent <- filter(metadata,parent != "" | parent %in% vars) %>%
                  filter(variable %in% vars)
  
  x %in% meta_parent$variable && !x %in% meta_parent$parent
  
}


gsum <- function(x) {  
  
  data %>% 
    {if(!noparent) dplyr::filter(.,!!sym(bottom) != !!sym(parent)) else .} %>%
    dplyr::group_by(dplyr::across(all_of(x)), .drop=TRUE) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup()
  
}

c2 <- combos

noparent <- FALSE

bottom <- vars[sapply(vars,is_bottom)]

i <- 1

while(!noparent){
  
  parent <- filter(metadata,variable==bottom) %>%
    pull(parent)
  
  noparent <- parent == ""
  
  c3 <- if(!noparent) {
    c2[sapply(c2,function(x){bottom %in% x})]
  } else {c2}
  
  new_table <- lapply(c3,gsum) %>%
    dplyr::bind_rows()
  
  if (i == 1){
    
    part_table <- new_table
    
  } else {
    
    part_table <- bind_rows(part_table,new_table)
    
  }
  
  
  if (noparent == FALSE){
  
  vars <- vars[!sapply(vars,is_bottom)]
  
  c2 <- c2[!sapply(c2,function(x){bottom %in% x})]
  
  bottom <- parent
  
  i <- i+1
  
  }

  
}




noparent <- FALSE

bottom <- vars[sapply(vars,is_bottom)]

parent <- filter(metadata,variable==bottom) %>%
  pull(parent)

while(!noparent) {

noparent <- parent == ""

combos2 <- combos[sapply(combos,function(x){bottom %in% x})]

vc<-lapply(combos2,gsum) %>%
  dplyr::bind_rows()

if (i==1) {
  
  fin <- vc
  
} else {
  
  fin <- bind_rows(fin,vc)
  
}

if (!noparent){

vars <- vars[!sapply(vars,is_bottom)]

combos <- combos[!sapply(combos,function(x){bottom %in% x})]
}

bottom <- parent

parent <- filter(metadata,variable==bottom) %>%
  pull(parent)

}









has_parent <- TRUE

varlist <- list(bottom)

while(has_parent == TRUE) {
  
  newbottom <- filter(meta,variable == bottom) %>%
    pull(parent)
  
  newitem <- c(varlist[[length(varlist)]],newbottom)
  
  varlist[[length(varlist)+1]] <- newitem
  
  newbottom_meta <- filter(meta,variable==newbottom)
  
  has_parent <- newbottom_meta$parent != "" && newbottom_meta$parent %in% vars
  
}





  
  
  filter(meta, parent != "" && parent %in% v)


children <- filter(meta,parent %in% vars) %>%
            pull(variable)


  
start <- "custody_group"

child <- filter(meta,parent == start) %>%
  pull(variable)

full <- c(start,child)


bottom <-vars[sapply(vars,function(x){x %in% meta$variable && !x %in% meta$parent})]





pair <- meta[1,]

pairchild <- filter(meta,parent==pair$variable) %>%
              pull(variable)

paircombo <- 







children <- dplyr::filter(meta,parent != "") %>%
            dplyr::pull(variable)

top <-vars[sapply(vars,function(x){x %in% meta$parent && !x %in% children})]

nextchild <- dplyr::filter(meta,parent == top) %>%
              dplyr::pull(variable)


combos[!sapply(combos,function(x){nextchild %in% x})]


mv <- filter(meta,variable %in% vars)


bottom <-vars[sapply(vars,function(x){x %in% mv$variable && !x %in% mv$parent})]

child <- filter(meta,parent == top) %>%
          select(variable) %>%
          as.character()

if (child != bottom) {
  
  top <- child
  
  child <- filter(meta,parent == top) %>%
    select(variable) %>%
    as.character()
  
  
}




combos <- make_combos(vars,meta)


vars %in% meta$variable && vars %in% meta$parent






  

