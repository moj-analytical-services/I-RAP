#' Build a prison receptions dataset in iRAP format
#'
#' This function builds an iRAP formatted data file of prison receptions
#' It is a specific form of the iRAP_build_data() function with specified parameters for the prison receptions data file
#'
#' @param dates date(s) of the file(s) you wish to use as source data files. Should be a character vector of dates in the format "yyyyQq" 
#' @return a data frame of prisonreceptions data
#' @export

prison_receptions_data <- function(dates, SHA="main") {
  
  changes <- list(subsequent_jisllen_band=tibble::tibble(old="L Unknown length - determinate sentence",
                                                new="Unknown"))
  
  custody_lookup <- tibble::tibble(untried_flag = c(1,NA,NA),
                           convicted_flag = c(NA,1,NA),
                           civil_flag = c(NA,NA,1),
                           custody_code = c("Remand untried","Remand convicted","Civil"))
  
  recepdata <- iRAP_build_data(datasource = "prison_receptions",
                               dates = dates,
                               in_vars = c("Sex", "Subsequent_JISLlen_Band", "Nationality_Code","Recep_Estab_ID","age","Main_Offence_Group","First_Flag",
                                           "Untried_Flag","Convicted_Flag","Civil_Flag"),
                               renames = dplyr::bind_cols(old_name=c("sex", "subsequent_jisllen_band", "recep_estab_id","main_offence_group"),
                                                          new_name=c("sex_code","custody_code","prison_code","offence_code")),
                               replace = changes,
                               lookups = c("sex","custody","nationality","prison","offence","first_reception"),
                               custom_lookups = list(custody_lookup),
                               age_lookups = "all",
                               join_vars = list(c("untried_flag","convicted_flag","civil_flag"),
                                                NULL,NULL,NULL,NULL,NULL,NULL),
                               count_indicator = "receptions",
                               SHA = SHA)
  
  return(recepdata)
  
}