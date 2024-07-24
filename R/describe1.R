# Descriptive statistics for overall
#' @export
describe1 <- function(DATA,CONDIGITS=1,CATDIGITS=1,FUNC="mean",TYPE=1){
  vals <- c()
  for(var in names(DATA)){
    if(var %in% var.num){
      if(FUNC=="mean"){ val <- DATA[[var]] %>% msd(digits = CONDIGITS, type = TYPE) }
      if(FUNC=="median"){ val <- DATA[[var]] %>% miqr(digits = CONDIGITS, type = TYPE) }
    }
    if(var %in% var.fac){
      val <- DATA[[var]] %>% nperc1(digits = CATDIGITS)
    }
    vals <- c(vals, val)
  }
  mat <- matrix(vals, ncol=1)
  colnames(mat) <- "overall"
  return(mat)
}
