#' @export
chisq_test <- function(DATA, DV, IV){
  ss <- DATA[c(IV, DV)]
  xt <- table(ss)
  res <- chisq.test(xt)
  pval <- res$p.value
  return(res)
}
