#' Build a prison receptions dataset in iRAP format
#'
#' This function builds an iRAP formatted data file of prison receptions
#' It is a specific form of the iRAP_build_data() function with specified parameters for the prison receptions data file
#'
#' @param dates date(s) of the file(s) you wish to use as source data files. Should be a character vector of dates in the format "yyyyQq" 
#' @return a data frame of prisonreceptions data
#' @export

prison_receptions_data <- function(dates, SHA="main") {
  
  recepdata <- iRAP_build_data(datasource = "prison_receptions",
                               dates = dates,
                               in_vars = c("Sex", "Subsequent_JISLlen_Band", "Nationality_Code","Recep_Estab_ID","age","Main_Offence_Group","First_Flag"),
                               renames = dplyr::bind_cols(old_name=c("sex", "subsequent_jisllen_band","recep_estab_id","main_offence_group"),
                                                          new_name=c("sex_code","custody_code","prison_code","offence_code")),
                               lookups = c("sex","custody","nationality","prison","offence","first_reception"),
                               age_lookups = "all",
                               indicator = "receptions",
                               SHA = SHA)
  
  return(recepdata)
  
}