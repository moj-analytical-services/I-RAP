#' Build a prison population data file from raw SAS files
#'
#' This function builds a prison population data file. 
#' It can combine multiple files based on population extracts taken on different dates.
#'
#' @param dates date of the population file(s) you wish to build a complete file from. Should be a character vector of dates in the format "yyyymmdd" 
#' @return A data frame of the prison population for each date selected
#' @export

prison_pop_data <- function(dates) {
  
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
                                                      prison_code = estab))

  ## Add additional variable recodes from lookup tables
  
  # Define paths to lookup tables in S3
  
  lookup_paths <- paste0("alpha-harmonisation/lookups/",c("sex",
                                                          "custody",
                                                          "ethnicity",
                                                          "nationality",
                                                          "prison"),".csv")
  
  # Define join varaibles for each lookup
  
  join_vars <- c("sex_code","custody_code","ethnicity_code","nationality_code","prison_code")

  
  # Loop through each lookup table matching it to the core data file

  for (i in 1:length(lookup_paths)) {
    
    # Import lookup file

    suppressMessages(
      
      lookup <- paws_read_using(FUN = readr::read_csv,
                          path = lookup_paths[i])
    )
    
    # Convert lookup table columns to character
    
    lookup <- data.frame(lapply(lookup, as.character), stringsAsFactors=FALSE)
    
    # Ensure blank cells in original lookup are blank in R data frame
    # This is needed for some 'Not recorded' codes, otherwise R reads them as NA
    
    lookup[is.na(lookup)] <- ""
    
    # Left join the prison population data to each lookup to add new variables

      if (i == 1) {
        
        finaldata <- dplyr::left_join(all_population_data,lookup,by=join_vars[i])
        
      } else {
        
        finaldata <- dplyr::left_join(finaldata,lookup,by=join_vars[i])
        
      }

  }

  ## Simple variables to be added or recoded
  
  finaldata$offence_group <- stringr::str_sub(finaldata$offence_group,4)
  
  finaldata$geographic_level <- "National"
  finaldata$country_code <- "K04000001"
  finaldata$country_name <- "England and Wales"
  
  finaldata$age <- as.numeric(finaldata$age)
  
  # Create age groups
    
  finaldata <- finaldata %>% 
                dplyr::mutate(
                  age_group = dplyr::case_when(
                    age >= 15 & age <= 17 ~ "15-17",
                    age >= 18 & age <= 20 ~ "18-20",
                    age >= 21 & age <= 24 ~ "21-24",
                    age >= 25 & age <= 29 ~ "25-29",
                    age >= 30 & age <= 39 ~ "30-39",
                    age >= 40 & age <= 49 ~ "40-49",
                    age >= 50 & age <= 59 ~ "50-59",
                    age >= 60 & age <= 69 ~ "60-69",
                    age >= 70  ~ "70 and over"
                  ))
  
  # Create another age grouping variable
  
  finaldata <- finaldata %>% 
    dplyr::mutate(
      age_group_adult = dplyr::case_when(
        age >= 15 & age <= 17 ~ "15-17",
        age >= 18 & age <= 20 ~ "18-20",
        age >= 21 ~ "Adult"
      ))
  
  # Aggregate data by all retained variables to compress data frame
  
  suppressMessages(
                
  finaldata <- finaldata %>%
    dplyr::group_by_at(c(names(finaldata)[!names(finaldata) %in% c(join_vars,"age")],
                  "age_group", "age_group_adult")) %>%
    dplyr::summarise(prisoners = dplyr::n())
  
  )
      
  return(finaldata)

}
