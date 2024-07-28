#' @export
normality.num <- function(DATA, VARIABLE) {
  nn <- length(VARIABLE)  # Added this line to define nn
  if (nn < 5000) {
    pval <- shapiro.test(DATA[[VARIABLE]])$p.value
    res <- (pval <= 0.05)
  } else {
    pval <- ks.test(DATA[[VARIABLE]], pnorm)$p.value
    res <- (pval <= 0.05)
  }
  return(res)
}
