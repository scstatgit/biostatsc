# Descriptive statistics for overall
#' @export
describe1 <- function(DATA, var.num, var.fac, CONDIGITS = 1, CATDIGITS = 1, FUNC = "mean", TYPE = 1){
  vals <- c()
  for(var in c(var.num, var.fac)){
    if(var %in% var.num){
      if(FUNC=="mean"){ val <- DATA[[var]] %>% var_msd(digits = CONDIGITS, type = TYPE) }
      if(FUNC=="median"){ val <- DATA[[var]] %>% var_miqr(digits = CONDIGITS, type = TYPE) }
    } else if(var %in% var.fac){
      val <- DATA[[var]] %>% var_nperc1(digits = CATDIGITS)
    } else {
      next
    }
    vals <- c(vals, val)
  }
  mat <- matrix(vals, ncol = 1)
  colnames(mat) <- paste("Overall", paste0("(n=",formatC(nrow(DATA), format="f", big.mark=",", digits=0),")"), sep=" ")
  return(mat)
}
