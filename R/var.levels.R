#' @export
var.levels <- function(DATA, var){
  vv <- var
  vv.nlevels <- plyr::count(DATA[[vv]])
  res <- vv.nlevels$x
  return(res)
}
