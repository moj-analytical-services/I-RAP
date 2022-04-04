#' Build a prison population dataset in iRAP format
#'
#' This function builds an iRAP formatted data file of the prison population
#' It is a specific form of the iRAP_build_data() function with specified parameters for the prison population data file
#'
#' @param dates date(s) of the file(s) you wish to use as source data files. Should be a character vector of dates in the format "yyyymmdd" 
#' @param SHA SHA for the lookup files you want to use
#' @return a data frame of prison population data
#' @export
#' @importFrom magrittr "%>%"

prison_pop_data <- function(dates, SHA="main") {
  
  popdata <- iRAP_build_data(datasource = "prison_pop",
                             dates = dates,
                             in_vars = c("Sex","custype","AGE","Offence_group","ETHNIC_GROUP_SHORT","NATIONALITY_SHORT","ESTAB","Offence_group"),
                             renames = dplyr::bind_cols(old_name = c("sex","custype","ethnic_group_short","nationality_short","estab","offence_group"),
                                                        new_name = c("sex_code","custody_code","ethnicity_code","nationality_code","prison_code","offence_code")),
                             lookups = c("sex","custody_population","ethnicity","nationality","prison","offence"),
                             age_lookups = "all",
                             count_indicator = "prisoners",
                             SHA = SHA)
  
  return(popdata)
  
}