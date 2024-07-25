#' @export
nperc <- function(DATA, var1, var2, digits = 0, type = 0){
  DATA <- DATA
  xt <- table(DATA[[var1]], DATA[[var2]])
  pt0 <- prop.table(xt, margin=NULL)
  pt1 <- prop.table(xt, margin=1) # row
  pt2 <- prop.table(xt, margin=2) # col
  if(type==0){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, pt0*100)
  }
  if(type==1){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, t(pt1)*100)
  }
  if(type==2){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, t(pt2)*100)
  }
  return(res)
}
