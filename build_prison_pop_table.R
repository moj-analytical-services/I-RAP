prison_pop_table <- function(tabledata,filtervars,nestedvars = NULL) {
  
  # Define variables that will be constant for every row of table
  
  geovars <- c("geographic_level",
                 "country_code",
                 "country_name")
  
  timevars <- c("time_period",
                 "time_identifier")
  
  geovar_all <- paste(geovars,collapse=".")
  
  timevar_all <- paste(timevars,collapse=".")
  
  # create combination of filter and nested vars
  
  if (is.null(nestedvars)) {
      combovars <- filtervars 
    } else {
      combovars <- c(filtervars,unlist(nestedvars))  
    }
  
  # Create a list of every possible combination of variables in the filtervar list
  # This will be used later to create an aggregated data frame which aggregates every combination of variables.
  # This allows the creation of 'Total' rows in the final table

  filterlist <- 1:length(combovars) %>% 
    purrr::map(
      ~
        gtools::combinations(length(combovars),.x,combovars)
    ) %>% 
      lapply(FUN=function(x) split(x,1:nrow(x)))
  
  filterlist <- do.call(c,filterlist) %>%
                append("")
  
  full_list <- filterlist %>%
    lapply(FUN=function(x) c(timevar_all,geovar_all,x)) %>%
    lapply(FUN=function(x) x <- x[x!=""]) %>%
    lapply(FUN=unlist)
  
  # Add factors to population data frame (needed in grouping step next)
  
  tabledata <- unite(tabledata,!!geovar_all,all_of(geovars),sep="!")
  tabledata <- unite(tabledata,!!timevar_all,all_of(timevars),sep="!")
  
  tabledata <- as.data.frame(unclass(tabledata), stringsAsFactors=TRUE)
  
  # Loop through summarising the data by every combination of filter variables
  # This means that every variable is totalled by every other
  # Creates a final data set with every possible combination of totals
  
  tabulate <- function(x){tabledata %>%
      dplyr::group_by_at(x, .drop=FALSE) %>%
      dplyr::summarise(prisoners = sum(prisoners))}
  
  suppressMessages(
  
  table <- lapply(full_list,FUN=tabulate) %>%
    bind_rows() 

  )
  
  table <- separate(table,timevar_all,timevars,sep = "!")
  table <- separate(table,geovar_all,geovars,sep = "!")
  
  # Remove factors created at earlier step
  
  table <- data.frame(lapply(table,as.character), stringsAsFactors=FALSE)
  
  # Define function that creates list of acceptable combinations of nested variables
  
  nested <- function(x){
    
    for (i in 0:(length(x)-2)) {
      
      if (i==0) {nestlist <- table}
      
      nestlist <- filter(nestlist,!is.na(!!as.name(x[length(x)-(i+1)])) | is.na(!!as.name(x[length(x)-i])), prisoners > 0)
      
    }
    
    nestlist <- unique(select(nestlist,contains(x)))
    
    return(nestlist)
    
  }
  
  # Apply function across all nested variables
  
  if (!is.null(nestedvars)) {
  
    valid_list <- lapply(nestedvars,FUN=nested)
  
    for (i in 1:length(valid_list)) {
      
      suppressMessages(
      
          table <-inner_join(table,valid_list[[i]])
          
      )
    
    }
    
  }
  
  # Re-code nested variables so that any without lower levels have "Total" across lower levels
  
  nest_totals <- function(x) {
    
    for (i in 1:length(x)) {
      
      varlist <- x[[i]] 
      
      for (j in 0:(length(varlist)-2)) {
        
        if (i==1 && j==0) {nestlist <- table}
        
        nestlist[[varlist[length(varlist)-j]]][nestlist[[varlist[length(varlist)-(j+1)]]] == nestlist[[varlist[length(varlist)-j]]]] <- "Total"
        
      }
      
    }
    
    return(nestlist)
    
  }
  
  
  if (!is.null(nestedvars)) {
    
    table <- nest_totals(nestedvars)
    
  }
  
  # Any cells with NA are totals
  # This is because they weren't included in the list to summarise by
  
  table[is.na(table)] <- "Total"
  
  # Creates some duplicate rows based on adding Totals for nested variables earlier
  
  table <- unique(table)
  
  # Make value column numeric
  
  table$prisoners <- as.numeric(table$prisoners)
  
  table <- select(table,-prisoners,prisoners)
  
  return(table)
  
}






