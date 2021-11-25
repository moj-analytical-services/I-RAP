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
#' @param keepvars a character vector of variables you want to keep from the original dataset
#' @param indicator a string showing the name you want to give to the final count variable
#' @param datasource a string showing which data source you want to use. The only accepted value currently is 'prison_pop'
#' @return A data frame in iRAP format
#' @export
#' @importFrom magrittr "%>%"

iRAP_build_data <- function(dates,lookups,join_vars,agespecs=NULL,renames,keepvars,indicator,datasource,SHA="main") {
  
  ## Define function for reading in individual datasets
  
  getdata <- function(date,datasource) {
    
    # Read datasets from raw files as specified in setsource.R
    
    raw_data <- readsource(date,datasource)

    # Add time variables
    
    if (datasource == "prison_pop") {
    
      raw_data$time_period <- stringr::str_sub(date,1,4)
      raw_data$time_identifier <- months(as.Date(date,format="%Y%m%d"))
      
      keepvars <- c(keepvars,"time_period","time_identifier")

    } else if (datasource == "prison_receptions") {
      
      raw_data$time_period <- stringr::str_sub(date,1,4)
      raw_data$time_identifier <- "Calendar year"
      raw_data$quarter <- stringr::str_to_upper(stringr::str_sub(date,5,6))
      
      keepvars <- c(keepvars,"time_period","time_identifier","quarter")

    }
    
    return(raw_data)
    
  }
  
  # Loop through all dates combining all raw datasets
  
  all_data <- dplyr::bind_rows(lapply(dates,FUN=getdata,datasource=datasource))

  # Convert variable names to lower case

  names(all_data) <- tolower(names(all_data))
  
  # Convert all columns to character
  
  all_data <- all_data %>% dplyr::mutate(dplyr::across(.fns = as.character))
  
  # Select limited range of variables for tables
  
  all_data <- all_data %>% dplyr::select(dplyr::any_of(keepvars))
  
  # Rename original variables
  
  all_data <- all_data %>% dplyr::rename_with(.cols = dplyr::all_of(renames$old_name),
                                              .fn = ~ renames$new_name)

  # Match lookup variables to main dataset
  
  joined_data <- join_lookups(all_data,lookups,join_vars,SHA)
  
  ## Add geographical variables
  
  joined_data$geographic_level <- "National"
  joined_data$country_code <- "K04000001"
  joined_data$country_name <- "England and Wales"
  
  # Create age groups
  
  if (!is.null(agespecs)) {
  
      joined_data$age <- as.numeric(joined_data$age)
        
      for (i in 1:length(agespecs)) {
        
        joined_data[[names(agespecs[i])]] <- cut(joined_data$age,breaks=c(agespecs[[i]]$start_age,999),
                                            labels=agespecs[[i]]$label,
                                            right=FALSE)
        
      }
      
      join_vars <- c(join_vars,"age")
  
  }
  
  # Aggregate data by all retained variables to compress data frame

  suppressMessages(
                
  finaldata <- joined_data %>%
    dplyr::group_by(dplyr::across(c(names(joined_data)[!names(joined_data) %in% c(join_vars)]))) %>%
    dplyr::summarise(countvar = dplyr::n())
  
  )
  
 finaldata <- finaldata %>% dplyr::rename_with(.cols = "countvar",
                                                .fn = ~ indicator)
      
  return(finaldata)

}
