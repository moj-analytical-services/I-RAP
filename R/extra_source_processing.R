prison_receptions_extra <- function(data) {
  
  receptions_data <- varcombine(data,"age",c("Untried_Age", "Sentenced_Age", "Recall_Age", "Convicted_Age", "First_Age"), "mean",0)
  
  receptions_data <- receptions_data %>% dplyr::mutate(Time_Months_Unround = Time_Inc_Days/(365.25/12))

  return(receptions_data)

}