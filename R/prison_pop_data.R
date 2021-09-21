#' Build a prison population data file from raw SAS files
#'
#' This function builds a prison population data file. 
#' It can combine multiple files based on population extracts taken on different dates.
#'
#' @param dates date of the population file(s) you wish to build a complete file from. Should be a character vector of dates in the format "yyyymmdd" 
#' @return A data frame of the prison population for each date selected
#' @export

prison_pop_data <- function(dates,agespecs) {
  
## Read datasets from SAS files
  
  for (i in 1:length(dates)) {

    s3path <- stringr::str_interp("alpha-dag-omsq/population/ao${dates[i]}.sas7bdat")

    population_data <- paws_read_using(FUN = haven::read_sas,
                                path = s3path)
    
    # Convert all columns to character

    population_data <- data.frame(lapply(population_data, as.character), stringsAsFactors=FALSE)
    
    # Add time variables
    
    population_data$time_period <- stringr::str_sub(dates[i],1,4)
    population_data$time_identifier <- months(as.Date(dates[i],format="%Y%m%d"))

    # Combine all selected population files into a single data frame
    
    if (i == 1) {

      all_population_data <- population_data

    } else {

      all_population_data <- dplyr::bind_rows(all_population_data,population_data)

    }

  }
  
  ## Simple formatting changes
  
  # Convert variable names to lower case

  names(all_population_data) <- tolower(names(all_population_data))
  
  # Select limited range of variables for tables

  all_population_data <- dplyr::select(all_population_data,
                                c(time_period,
                                  time_identifier,
                                  sex,
                                  custype,
                                  age,
                                  offence_group,
                                  ethnic_group_short,
                                  nationality_short,
                                  estab,
                                  offence_group))
  
  # Rename original variables

  all_population_data <- dplyr::rename(all_population_data,c(sex_code = sex,
                                                      custody_code = custype,
                                                      ethnicity_code = ethnic_group_short,
                                                      nationality_code = nationality_short,
                                                      prison_code = estab,
                                                      offence_code = offence_group))

  ## Add additional variable recodes from lookup tables
  
  # Define join variables for each lookup and list of lookups
  
  lookups <- c("sex","custody","ethnicity","nationality","prison","offence")
  
  join_vars <- c("sex_code","custody_code","ethnicity_code","nationality_code","prison_code","offence_code")

  # Match lookup variables to main dataset
  
  finaldata <- join_lookups(all_population_data,lookups,join_vars)
  
  ## Simple variables to be added or recoded
  
  finaldata$geographic_level <- "National"
  finaldata$country_code <- "K04000001"
  finaldata$country_name <- "England and Wales"
  
  # Create age groups
  
  finaldata$age <- as.numeric(finaldata$age)
    
  for (i in 1:length(agespecs)) {
    
    finaldata[[names(agespecs[i])]] <- cut(finaldata$age,breaks=c(agespecs[[i]]$start_age,999),
                                        labels=agespecs[[i]]$label,
                                        right=FALSE)
    
  }
  
  # Aggregate data by all retained variables to compress data frame
  
  suppressMessages(
                
  finaldata <- finaldata %>%
    dplyr::group_by_at(c(names(finaldata)[!names(finaldata) %in% c(join_vars,"age")],
                  "age_group", "age_group_adult")) %>%
    dplyr::summarise(prisoners = dplyr::n())
  
  )
      
  return(finaldata)

}
