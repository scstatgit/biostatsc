#' @export
normality_test <- function(DATA, DV, IV){
  mf <- as.formula(paste(DV,IV,sep=" ~ "))
  fit <- lm(mf, data = DATA)
  if(nrow(DATA)<5000){
    res <- shapiro.test(resid(fit))$p.value
    res <- ifelse(res < 0.05, FALSE, TRUE) # TRUE: assumption met, FALSE: assumption does not met
  } else {
    res <- ks.test(resid(fit), "pnorm", mean(resid(fit)), sd(resid(fit)))$p.value
    res <- ifelse(res < 0.05, FALSE, TRUE) # TRUE: assumption met, FALSE: assumption does not met
  }
  return(res)
}
