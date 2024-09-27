#' @export
chisq_assumption <- function(DATA, DV, IV){
  ss <- DATA[c(IV, DV)]
  xt <- table(ss)
  expected <- outer(marginSums(xt,1), marginSums(xt,2))/sum(xt)
  assumption <- ifelse(sum(expected<5)/prod(dim(xt)) >= 0.2, FALSE, TRUE) # TRUE: assumption met, FALSE: assumption does not met
  return(assumption)
}
