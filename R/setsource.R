# Function to set up instructions for importing data from different data sources
# Specifies file path, file format and any bespoke extra processing steps
# Returns a list of these values

setsource <- function(date, datasource) {
  
  if (datasource == "prison_pop") {
    
    s3path <- stringr::str_interp("alpha-published-omsq/population/ao${date}.sas7bdat")
    format <- "SAS"
    extra_processing <- NA
  }
  
  else if (datasource == "prison_receptions") {
    
    s3path <- stringr::str_interp("alpha-published-omsq/receptions/receps${date}.sas7bdat")
    format <- "SAS"
    extra_processing <- "prison_receptions_extra"
  }
  
  else if (datasource == "prison_releases") {
    
    s3path <- stringr::str_interp("alpha-published-omsq/releases/releases${date}.sas7bdat")
    format <- "SAS"
    extra_processing <- NA
  }
  
  else {
    
    stop("datasource not recognised")
    
  }
  
  return(list(s3path=s3path, format=format, extra_processing=extra_processing))
  
}