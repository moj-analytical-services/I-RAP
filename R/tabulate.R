#' Build an iRAP Table
#'
#' This function take an iRAP data file, produced via the I-RAP::iRAP_build_data() function and produces an iRAP table.
#' It will provide a breakdown of the data by specified varaibles, including creating 'Total' rows.
#'
#' @param data An iRAP data file produced by I-RAP::iRAP_build_data()
#' @param vars A character vector of variables you wish to include in the table
#' @param metadata A metadata table you wish to read from
#' @param indicator The indicator variable from the table
#' @return A data table containing the final iRAP table
#' @export
#' @importFrom magrittr "%>%"

tabulate <- function(data,vars,metadata,indicator) {
  
  combinations <- list_combinations(vars,metadata)
  
  geovars <- c("geographic_level",
              "country_code",
              "country_name")

  timevars <- c("time_period",
              "time_identifier")

  combinations <- lapply(combinations,function(x){append(x,c(geovars,timevars))})
  
  combinations <- c(combinations,list(c(geovars,timevars)))
  
  root_var <- lapply(combinations,find_root_var,metadata=metadata)
  
  parent_var <- lapply(root_var,function(var){sapply(var,find_parent,metadata=metadata)})
  
  table <- purrr::pmap(list(root_var,parent_var,combinations),summarise_by_combination,data=data,indicator=indicator) %>%
            dplyr::bind_rows()
   
  table[is.na(table)] <- "Total"
  
  return(table)
  
}