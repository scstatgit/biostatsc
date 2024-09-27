#' @export
format_pval <- function(x, digits){
  res <- format.pval(round(x, digits), digits = digits, eps = as.numeric(paste0("1E-",digits)), nsmall = digits)
  return(res)
}
