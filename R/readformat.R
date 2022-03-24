readformat <- function(s3path,format) {
  
  if (format == "SAS") {
  
  data <- botor::s3_read(paste0("s3://",s3path),
                         haven::read_sas)
  } 
  
  return(data)
  
}