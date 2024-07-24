#' @export
varfac <- function(DATA){
  res <- sapply(DATA, is.factor)[sapply(DATA, is.factor)] %>% names()
  return(res)
}
