#' @export
var.levels <- function(DATA, var){
  xt <- table(DATA[[var]]) %>% as.data.frame()
  names(xt) <- c("x", "freq")
  res <- xt$x
  return(res)
}
