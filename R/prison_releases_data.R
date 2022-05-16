#' Build a prison releases dataset in iRAP format
#'
#' This function builds an iRAP formatted data file of prison releases
#' It is a specific form of the iRAP_build_data() function with specified parameters for the prison releases data file
#'
#' @param dates date(s) of the file(s) you wish to use as source data files. Should be a character vector of dates in the format "yyyyQq" 
#' @return a data frame of prison releases data
#' @export

prison_releases_data <- function(dates, SHA="main") {


  releasedata <- iRAP_build_data(datasource = "prison_releases",
                                 dates = dates,
                                 in_vars = c("Sex","Discharge_Age","Prior_JISLlen_Band","Avesen"),
                                 renames = dplyr::bind_cols(old_name=c("sex", "prior_jisllen_band", "discharge_age"),
                                                            new_name=c("sex_code","custody_code","age")),
                                 lookups = c("sex","custody_releases"),
                                 age_lookups = "all",
                                 count_indicator = "releases",
                                 mean_indicator = "mean_sentence_length",
                                 mean_indicator_var = "avesen",
                                 SHA=SHA
                                 )

  return(releasedata)
  
}