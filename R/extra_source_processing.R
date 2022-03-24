prison_receptions_extra <- function(data) {
  
  receptions_data <- varcombine(data,"age",c("Untried_Age", "Sentenced_Age", "Recall_Age", "Convicted_Age", "First_Age"), "mean",0)

  return(receptions_data)

}