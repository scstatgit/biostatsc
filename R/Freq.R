#' @export
Freq <- function(x, digits) {
  lvs <- levels(droplevels.factor(x))
  freq <- tabulate(x)
  percent <- prop.table(freq)
  res <- sprintf(paste0("%s: %.f (%.",digits,"f%%)"), lvs, freq, percent*100) %>% toString()
  return(res)
}
