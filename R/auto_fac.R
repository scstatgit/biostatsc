#' @export
auto_fac <- function(DATA, var.lvs){
  for(var in names(var.lvs)){
    if(var.lvs[[var]] <= 5){
      DATA[[var]] <- as.factor(DATA[[var]])
    }
  }
  return(DATA)
}
