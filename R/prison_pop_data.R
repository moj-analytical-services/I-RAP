#' Build a prison population dataset in iRAP format
#'
#' This function builds an iRAP formatted data file of the prison population
#' It is a specific form of the iRAP_build_data() function with specified parameters for the prison population data file
#'
#' @param dates date(s) of the file(s) you wish to use as source data files. Should be a character vector of dates in the format "yyyymmdd" 
#' @return a data frame of prison population data
#' @export

prison_pop_data <- function(dates) {
  
  popdata <- iRAP_build_data(dates,
                             agespecs = read_lookups()$agespecs,
                             lookups = c("sex","custody","ethnicity","nationality","prison","offence"),
                             join_vars = c("sex_code","custody_code","ethnicity_code","nationality_code","prison_code","offence_code"),
                             renames = bind_cols(old_name = c("sex","custype","ethnic_group_short","nationality_short","estab","offence_group"),
                                                 new_name = c("sex_code","custody_code","ethnicity_code","nationality_code","prison_code","offence_code")),
                             keepvars = c("time_period","time_identifier","sex","custype","age","offence_group","ethnic_group_short","nationality_short","estab","offence_group"),
                             indicator = "prisoners",
                             datasource = "prison_pop")
  
  return(popdata)
  
}