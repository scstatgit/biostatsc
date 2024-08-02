#' @export
isNumerics <- function(DATA){
  res <- sapply(DATA, is.numeric)[sapply(DATA, is.numeric)]
  return(res)
}
