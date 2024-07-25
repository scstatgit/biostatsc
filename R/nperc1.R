#' @export
nperc1 <- function(var, digits = 0){
  xt <- table(var)
  pt0 <- prop.table(xt)
  res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, pt0*100)
  return(res)
}
