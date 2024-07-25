#' @export
nperc2 <- function(var, digits = 0, type = 0){
  xt <- table(var)
  pt0 <- prop.table(xt, margin=NULL)
  pt1 <- prop.table(xt, margin=1) # row
  pt2 <- prop.table(xt, margin=2) # col
  if(type==0){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, pt0*100)
  }
  if(type==1){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, pt1*100)
  }
  if(type==2){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, pt2*100)
  }
  return(res)
}
