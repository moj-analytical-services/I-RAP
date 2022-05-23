#' Build an iRap formatted data file from raw files
#'
#' This function builds an iRAP formatted data file. 
#' It combines any number of raw data files into a standard iRAP format
#' This function is only currently tested with prison population data
#'
#' @param dates date(s) of the file(s) you wish to use as source data files. Should be a character vector of dates in the format "yyyymmdd" 
#' @param lookups a character vector of the lookups you want to use for creating new variables
#' @param join_vars a character vector of variable names that you want to use to join to the lookup tables
#' @param agespecs a list containing data frame(s) each of which specifies age bands to use
#' @param renames a data frame showing the old and new names of variables to rename
#' @param in_vars a character vector of variables you want to keep from the original dataset
#' @param indicator a string showing the name you want to give to the final count variable
#' @param datasource a string showing which data source you want to use. The only accepted value currently is 'prison_pop'
#' @return A data frame in iRAP format
#' @export
#' @importFrom magrittr "%>%"

iRAP_build_data <- function(datasource,dates,in_vars=NULL,renames=NULL,replace=NULL,lookups=NULL,custom_lookups=NULL,
                            age_lookups=NULL,join_vars=NULL,count_indicator,mean_indicator=NULL,mean_indicator_var=NULL,SHA="main") {
  
  # Loop through all dates combining all raw datasets
  
  all_data <- dplyr::bind_rows(lapply(dates,FUN=readsource,datasource=datasource,in_vars=in_vars))

  # Convert variable names to lower case

  names(all_data) <- tolower(names(all_data))
  
  # Convert all columns to character
  
  all_data <- all_data %>% dplyr::mutate(dplyr::across(.fns = as.character))
  
  # Replace variable values
  
  if (!is.null(replace)) {
    
    all_data <- replace_values(all_data,replace)
    
  }
  
  # Rename original variables
  
  if (!is.null(renames)) {
  
    all_data <- all_data %>% dplyr::rename_with(.cols = dplyr::all_of(renames$old_name),
                                              .fn = ~ renames$new_name)
  }

  # Match lookup variables to main dataset
  
  if (!is.null(lookups) | !is.null(custom_lookups)) {
  
    all_data <- join_lookups(all_data,lookups,custom_lookups,join_vars,SHA)
  
  }
  
  ## Add geographical variables
  
  all_data$geographic_level <- "National"
  all_data$country_code <- "K04000001"
  all_data$country_name <- "England and Wales"
  
  # Create age groups
  
  if (!is.null(age_lookups)) {
    
    all_data <- add_age_groups(all_data,age_lookups,SHA)
    
    all_data <- dplyr::select(all_data,-age)
  
  }
  
  # Remove any factors
  
  all_data <- data.frame(lapply(all_data,as.character), stringsAsFactors=FALSE)
  
  # Aggregate data by all retained variables to compress data frame

  finaldata <- all_data %>%
    make_indicators(count_indicator,mean_indicator,mean_indicator_var) %>%
    droplevels() %>%
    dplyr::ungroup()

      
  return(finaldata)

}
