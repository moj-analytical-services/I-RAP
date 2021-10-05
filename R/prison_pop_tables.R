#' Build a set of prison population data tables in iRAP format
#'
#' This function builds a set of iRAP formatted data tables of the prison population
#' It is a specific form of the iRAP_build_table() function with specified parameters for the standard published prison population tables
#'
#' @param dates date(s) of the file(s) you wish to use as source data files. Should be a character vector of dates in the format "yyyymmdd"
#' @return a list of data tables containing standard published data 
#' @export
#' @importFrom magrittr "%>%"

prison_pop_tables <- function(dates, SHA="main") {
  
  popdata <- prison_pop_data(dates, SHA)
  
  latestdate <- as.character(max(as.numeric(dates)))
  
  latestdata <- prison_pop_data(latestdate)
  
  custodyvars <- list(c("custody_group","custody_type","custody_detail","custody_sentence_detail"))
  
  all_tables <- list(
  
  Table1_1 = iRAP_build_table(popdata,c("sex","age_group_adult"),custodyvars,indicator = "prisoners"),
  
  Table1_2 = iRAP_build_table(dplyr::filter(popdata,custody_type %in% c("Untried","Convicted unsentenced") | (custody_group == "Sentenced" & custody_type !="Fine defaulter")),
                              c("sex","age_group_adult","offence_group"),
                              list(c("custody_group","custody_type")),
                              indicator="prisoners") %>%
              dplyr::filter(custody_group != "Sentenced" | (custody_group == "Sentenced" & custody_type =="Total")),
  
  Table1_3 = iRAP_build_table(popdata,c("sex","age_group","custody_group"),indicator="prisoners"),
  
  Table1_4 = iRAP_build_table(popdata,c("ethnicity_group","sex"),indicator="prisoners"),
  
  Table1_6 = iRAP_build_table(popdata,"nationality_group",custodyvars,indicator="prisoners"),
  
  Table1_7 = iRAP_build_table(latestdata,"sex",list(c("nationality_group","nationality_continent","nationality")),indicator = "prisoners"),
  
  Table1_8 = iRAP_build_table(latestdata,"nationality_group",list(c("prison_type","prison_name")),indicator = "prisoners")
  
  )
  
  return(all_tables)
  
}