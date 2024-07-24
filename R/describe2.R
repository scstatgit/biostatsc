# Descriptive statistics by strata
#' @export
describe2 <- function(DATA,STRATA,CONDIGITS=1,CATDIGITS=1,FUNC="mean",TYPE=1,MARGIN=2){
  vals <- c()
  for(var in names(DATA)){
    if(var %in% var.num){
      if(FUNC=="mean"){ val <- tapply(DATA[[var]], INDEX = DATA[[STRATA]], function(var) msd(var, digits = CONDIGITS, type = TYPE)) }
      if(FUNC=="median"){ val <- tapply(DATA[[var]], INDEX = DATA[[STRATA]], function(var) miqr(var, digits = CONDIGITS, type = TYPE)) }
    }
    if(var %in% var.fac){
      vec <- nperc(DATA, var, STRATA, type = MARGIN, digits = CATDIGITS)
      val <- matrix(vec, ncol=var.lvs[[STRATA]])
    }
    vals <- c(vals, val)
  }
  mat <- matrix(vals, ncol=var.lvs[[STRATA]], byrow=T)
  colnames(mat) <- as.character(var.levels(ddd,STRATA))
  return(mat)
}
