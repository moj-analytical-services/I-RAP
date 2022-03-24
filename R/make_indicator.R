make_indicators <- function(data,count_indicator,mean_indicator,mean_indicator_var) {
  
  data_indicator <- if (is.null(mean_indicator)) {
    
            data %>% 
              dplyr::group_by(dplyr::across(everything())) %>%
              dplyr::summarise(countvar = dplyr::n()) %>%
              dplyr::rename_with(.cols = "countvar",
                       .fn = ~ count_indicator)
      
          } else {
            
            data %>% 
              dplyr::group_by(dplyr::across(-!!rlang::sym(mean_indicator_var))) %>%
              dplyr::summarise(countvar = dplyr::n(),
                               meanvar = mean(as.numeric(!!rlang::sym(mean_indicator_var)),na.rm=TRUE)) %>%
              dplyr::rename_with(.cols = all_of(c("countvar","meanvar")),
                       .fn = ~ c(count_indicator,mean_indicator))
                            
          }

  return(data_indicator)
  
}