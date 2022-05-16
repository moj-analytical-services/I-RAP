#' Build a set of prison releases data tables in iRAP format
#'
#' This function builds a set of iRAP formatted data tables of prison receptions
#' It is a specific form of the iRAP_build_table() function with specified parameters for the standard published prison release tables
#'
#' @param dates date(s) of the file(s) you wish to use as source data files. Should be a character vector of dates in the format "yyyyQq"
#' @return a list of data tables containing standard published data 
#' @export

prison_releases_tables <- function(dates, SHA="main") {
  
  releases <- prison_releases_data(dates, SHA)
  
  custodyvars <- list(c("custody_group","custody_type","custody_detail","custody_sentence_detail"))
  
  all_tables <- list(
    
    Table3_1 = iRAP_build_table(releases,filtervars=c("sex","age_group_adult"),nestedvars=list(c("custody_group","custody_type","custody_detail","custody_sentence_detail")),nototalvars="custody_group",count_indicator="releases"),

    Table3_2i = iRAP_build_table(dplyr::filter(releases,custody_type=="Determinate sentences"),filtervars=c("sex"),nestedvars=list(c("custody_type","custody_detail","custody_sentence_detail")),nototalvars="custody_type",count_indicator="releases",mean_indicator="mean_sentence_length")
    
  )
  
  return(all_tables)
  
}