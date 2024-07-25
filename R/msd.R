#' msd() Function
#'
#' msd
#' @param var, digits, type
#' @keywords msd
#' @examples
#' msd()
#'
msd <- function(var,digits=3,type=1){
  if(type==1){
    res <- sprintf(paste0("%.",digits,"f"," \U00B1 %.",digits,"f"), mean(var, na.rm=TRUE), sd(var, na.rm=TRUE))
  }
  if(type==2){
    res <- sprintf(paste0("%.",digits,"f"," (%.",digits,"f)"), mean(var, na.rm=TRUE), sd(var, na.rm=TRUE))
  }
  return(res)
}
