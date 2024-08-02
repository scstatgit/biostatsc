#' @export
var_nlevels <- function(DATA, var){
  xt <- table(var) %>% as.data.frame()
  names(xt) <- c("x", "freq")
  res <- length(xt$x)
  return(res)
}
