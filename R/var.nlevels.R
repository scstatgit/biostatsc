#' @export
var.nlevels <- function(DATA, var){
  vv <- var
  vv.nlevels <- plyr::count(vv)
  res <- length(vv.nlevels$x)
  return(res)
}
