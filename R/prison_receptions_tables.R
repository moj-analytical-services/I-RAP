#' Build a set of prison receptions data tables in iRAP format
#'
#' This function builds a set of iRAP formatted data tables of prison receptions
#' It is a specific form of the iRAP_build_table() function with specified parameters for the standard published prison reception tables
#'
#' @param dates date(s) of the file(s) you wish to use as source data files. Should be a character vector of dates in the format "yyyyQq"
#' @return a list of data tables containing standard published data 
#' @export

prison_receptions_tables <- function(dates, SHA="main") {
  
  receptions <- prison_receptions_data(dates, SHA)
  
  first_receptions <- dplyr::filter(receptions,first_reception == "Yes")

  custodyvars <- list(c("custody_group","custody_type","custody_detail","custody_sentence_detail"))
  
  all_tables <- list(
    
    Table2_1 = iRAP_build_table(first_receptions,"sex",nestedvars=list(c("custody_group","custody_detail","custody_sentence_detail")),indicator="receptions"),
    
    Table2_2 = iRAP_build_table(first_receptions,nestedvars=list(c("nationality_group","nationality_continent","nationality")),indicator="receptions"),
    
    Table2_3 = iRAP_build_table(first_receptions,nestedvars=list(c("prison_type","prison_name")),indicator="receptions"),
    
    Table2_4a = iRAP_build_table(dplyr::filter(receptions,custody_group == "Remand first receptions"),c("custody_type","age_group_adult","sex"),indicator="receptions"),
  
    Table2_4b = iRAP_build_table(dplyr::filter(receptions,custody_group == "Remand first receptions"),c("custody_type","offence_group","sex"),indicator="receptions"),
    
    Table2_5a = iRAP_build_table(dplyr::filter(receptions,custody_group == "Sentenced first receptions" & custody_type != "Recalls"),c("age_group_adult","sex"),nestedvars=list(c("custody_type","custody_detail","custody_sentence_detail")),indicator="receptions"),
    
    Table2_5b = iRAP_build_table(dplyr::filter(receptions,custody_group == "Sentenced first receptions" & !custody_type %in% c("Recalls","Fine defaulter")),
                                               c("sex","offence_group"),indicator="receptions")
    
    )
  
  return(all_tables)
  
}