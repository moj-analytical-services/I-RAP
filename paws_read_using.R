paws_read_using <- function(path, FUN = readr::read_csv) {
  svc <- paws::s3(config=list(region="eu-west-1"))
  path_split <- stringr::str_split(path, '/')
  bucket <- path_split[[1]][1]
  key <- paste(path_split[[1]][-1],collapse="/")
  obj <- svc$get_object(Bucket=bucket, Key=key)
  return(obj$Body %>% FUN())
}