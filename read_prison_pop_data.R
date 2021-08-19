prison_pop_data <- function(dates) {

  for (i in 1:length(dates)) {

    s3path <- stringr::str_interp("alpha-dag-omsq/population/ao${dates[i]}.sas7bdat")

    population_data <- s3tools::read_using(FUN = haven::read_sas,
                                s3_path = s3path)

    population_data <- data.frame(lapply(population_data, as.character), stringsAsFactors=FALSE)
    
    population_data$time_period <- stringr::str_sub(dates[i],1,4)
    population_data$time_identifier <- months(as.Date(dates[i],format="%Y%m%d"))

    if (i == 1) {

      all_population_data <- population_data

    } else {

      all_population_data <- dplyr::bind_rows(all_population_data,population_data)

    }

  }

  names(all_population_data) <- tolower(names(all_population_data))

  all_population_data <- select(all_population_data,
                                c(time_period,
                                  time_identifier,
                                  sex,
                                  custype,
                                  age,
                                  offence_group,
                                  ethnic_group_short,
                                  nationality_short,
                                  estab))

  all_population_data <- rename(all_population_data,c(sex_code = sex,
                                                      custody_code = custype,
                                                      ethnicity_code = ethnic_group_short,
                                                      nationality_code = nationality_short,
                                                      prison_code = estab))

  lookup_paths <- paste0("alpha-harmonisation/lookups/",c("sex",
                                                          "custody",
                                                          "ethnicity",
                                                          "nationality",
                                                          "prison"),".csv")
  
  join_vars <- c("sex_code","custody_code","ethnicity_code","nationality_code","prison_code")


  for (i in 1:length(lookup_paths)) {

    suppressMessages(
      
      lookup <- s3tools::read_using(FUN = readr::read_csv,
                          s3_path = lookup_paths[i])
    )
    
    lookup <- data.frame(lapply(lookup, as.character), stringsAsFactors=FALSE)

      if (i == 1) {
        
        finaldata <- dplyr::left_join(all_population_data,lookup,by=join_vars[i])
        
      } else {
        
        finaldata <- dplyr::left_join(finaldata,lookup,by=join_vars[i])
        
      }

  }
  
  finaldata$offence_group <- stringr::str_sub(finaldata$offence_group,4)
  
  finaldata$geographic_level <- "National"
  finaldata$country_code <- "K04000001"
  finaldata$country_name <- "England and Wales"
  
  finaldata$age <- as.numeric(finaldata$age)
    
    finaldata %>% 
    mutate(
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
    
  finaldata <- finaldata %>%
    group_by_at(c(names(finaldata)[!names(finaldata) %in% c(join_vars,"age")],
                  "age_group")) %>%
    summarise(prisoners = n())
      
  return(finaldata)

}
