#' @export
fisher_test <- function(DATA, DV, IV, SIMULATE.P.VALUE = FALSE, B = NULL, WORKSPACE = 2e+05){
  ss <- DATA[c(IV, DV)]
  xt <- table(ss)
  res <-
    tryCatch(
      fisher.test(xt, workspace = 2e+05),
      warning = function(w) w,
      error = function(e) {
        if((SIMULATE.P.VALUE == TRUE) & (is.null(B) == TRUE)){
          fisher.test(xt, simulate.p.value = TRUE, B = 2000)
        } else if ((SIMULATE.P.VALUE == TRUE) & (is.null(B) == FALSE)){
          fisher.test(xt, simulate.p.value = TRUE, B = B)
        } else {
          fisher.test(xt, workspace = WORKSPACE)
        }
      },
      finally = NULL
    )
  pval <- res$p.value
  return(pval)
}
