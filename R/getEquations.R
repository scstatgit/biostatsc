getEquations <- function(model, digits=3){
  coefs <- coef(model)
  coefs.f <- ifelse(coefs>=0,paste0("+", sprintf(paste0("%.",digits,"f"), coefs)), sprintf(paste0("%.",digits,"f"), coefs))
  res <- paste0("Outcome = ",coefs.f[1],paste0(paste(coefs.f[-1],names(coefs.f)[-1], sep = "*"), collapse=""))
  return(res)
}
