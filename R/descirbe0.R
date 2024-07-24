# variables and levels
lst <- list()
#' @export
descirbe0 <- function(DATA,LVS){
  row.lvs <- LVS
  row.lvs[which(!(names(row.lvs) %in% var.fac))] <- 1
  for (var in names(DATA)){
    if(row.lvs[var]==1){
      res <- ""
    }
    if(row.lvs[var]>=2){
      res <- var.levels(DATA,var) %>% as.character()
    }
    idx <- which(names(row.lvs)==var)
    lst[[idx]] <- res
  }
  unlist(lst)
  nm <- rep(names(row.lvs), row.lvs)
  cumsum(row.lvs)
  nrow <- cumsum(row.lvs)[length(cumsum(row.lvs))]
  mat <- matrix("",ncol=2,nrow=nrow)
  mat[,1] <- nm
  mat[,2] <- unlist(lst)
  return(mat)
}
