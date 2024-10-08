#' @export
delong_test <- function(x, y){
  if(auc(y)-auc(x)>0){
    pval <- roc.test(x,y,method="delong")$p.value
  } else {
    pval <- NA
  }
  return(pval)
}
