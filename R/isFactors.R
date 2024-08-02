#' @export
isFactors <- function(DATA){
  res <- sapply(DATA, is.factor)[sapply(DATA, is.factor)]
  return(res)
}
