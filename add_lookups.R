add_lookups <- function(data,path,vars){

  if (length(path) != length(vars)){
    stop("Argument 'path' and argument 'vars' must be the same length.")
  }

  for (i in 1: length(path)) {

    lookup <- s3tools::read_using(FUN = readr::read_csv,
                                s3_path = path[i])

    data <- dplyr::left_join(data,lookup,by = var[i])

  }

  return(data)

}
