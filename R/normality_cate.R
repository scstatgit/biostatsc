#' @export
normality.cate <- function(DATA, STRATA, VARIABLE) {
  xt <- table(unlist(subset(DATA, select = STRATA)), unlist(subset(DATA, select = VARIABLE)))
  xt.e5 <- sum(chisq.test(xt)$expected < 5)
  xt.nn <- prod(dim(xt))
  if (xt.e5 / xt.nn < 0.2) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
