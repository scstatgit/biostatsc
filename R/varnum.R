#' @export
varnum <- function(DATA){
  res <- sapply(DATA, is.numeric)[sapply(DATA, is.numeric)] %>% names()
  return(res)
}
