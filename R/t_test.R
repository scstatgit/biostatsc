#' @export
t_test <- function(DATA, DV, IV){
  var.equal <- (car::leveneTest(DATA[[DV]], DATA[[IV]], center="median")[[3]][1] <= 0.05)
  mf <- as.formula(paste(DV,IV,sep=" ~ "))
  res <- t.test(mf, DATA, var.equal = var.equal)
  pval <- res$p.value
  return(pval)
}
