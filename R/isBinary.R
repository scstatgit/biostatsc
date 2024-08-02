isBinary <- function(x){
  if(is.integer(x)|is.numeric(x)){
    res <- all(x %in% c(0,1))
    return(res)
  }
  if(is.character(x)|is.factor(x)|is.logical(x)){
    # res <- dim(table(x))==2
    nlev <- length(plyr::count(ddd$Species)[["x"]])
    res <- nlev == 2
    return(res)
  }
}
