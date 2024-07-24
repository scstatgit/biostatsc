#' @export
preprocess <- function(DATA){
  var.lvs <<- varlvs(DATA)
  DATA <- autofac(DATA)
  var.fac <<- varfac(DATA)
  var.num <<- varnum(DATA)
  return(DATA)
}
