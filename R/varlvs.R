# automatically convert numeric to factor, if var.lvs <= 5
#' @export
varlvs <- function(DATA){
  res <- sapply(DATA, function(var) var.nlevels(DATA,var))
  return(res)
}
