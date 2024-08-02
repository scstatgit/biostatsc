#' @export
preprocess <- function(DATA){
  var.lvs <<- sapply(DATA, function(var) var_nlevels(DATA,var))
  DATA <- auto_fac(DATA)
  var.fac <<- isFactors(DATA) %>% names
  var.num <<- isNumerics(DATA) %>% names
  return(DATA)
}
