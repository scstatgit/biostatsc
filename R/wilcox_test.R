#' @export
wilcox_test <- function(DATA, DV, IV){
  mf <- as.formula(paste(DV,IV,sep=" ~ "))
  res <- wilcox.test(mf, DATA)
  pval <- res$p.value
  return(pval)
}
