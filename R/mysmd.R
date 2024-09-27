#' @export
mysmd <- function(varvalue, varname, groupvalue, groupname){
  if(nlevels(groupvalue)==2){
    res <- smd(x = varvalue, g = groupvalue)$estimate
  } else if(nlevels(groupvalue)>=3) {
    res <- smd(x = varvalue, g = groupvalue)$estimate %>% mean()
  } else {
    res <- NULL
  }
  data.frame(Characteristics = varname, smd = res)
}
