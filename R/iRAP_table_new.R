df <- iRAP::prison_pop_data("20201231")


iRAP_table_new <- function(data,vars,count_indicator) {
  
  varcombos <- append(as.list(vars),list(vars))
  
  data <- data %>% 
          dplyr::ungroup() %>%
          dplyr::select(all_of(c(vars,count_indicator)))
  
  tabulate <- function(x){data %>%
      dplyr::group_by(dplyr::across(x), .drop=TRUE) %>%
      dplyr::summarise("{count_indicator}" := sum(.data[[count_indicator]]),.groups="drop") %>%
      dplyr::ungroup()}

  table <- lapply(varcombos,FUN=tabulate) %>%
      dplyr::bind_rows(.,
                        dplyr::summarise(data,"{count_indicator}" := sum(.data[[count_indicator]]),.groups="drop")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(any_of(vars),~dplyr::case_when(is.na(.) ~ "Total", TRUE ~ .))) %>%
      dplyr::select(-count_indicator,count_indicator)
      
  
  return(table)
}


t2 <- iRAP_table_new(df,c("sex","ethnicity_group"),"prisoners")
