#' @export
miqr <- function(var,digits=3,type=1){
  quant <- quantile(var, na.rm=TRUE)
  if(type==1){
    res <- sprintf(paste0("%.",digits,"f"," [%.",digits,"f, %.",digits,"f]"), quant[3], quant[2], quant[4])
  }
  if(type==2){
    res <- sprintf(paste0("%.",digits,"f - %.",digits,"f"), quant[1], quant[5])
  }
  if(type==3){
    res <- sprintf(paste0("Median: %.",digits,"f\n","[Q1: %.",digits,"f, Q3: %.",digits,"f]\n",
                          "Range: %.",digits,"f - %.",digits,"f"), quant[3], quant[2], quant[4], quant[1], quant[5])
  }
  return(res)
}
