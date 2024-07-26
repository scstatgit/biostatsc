#' @export
MostCommon <- function(x) {
  ux <- unique(x)
  uxnotna <- ux[which(!is.na(ux))]
  if(length(uxnotna) > 0) {
    tab <- tabulate(match(x, uxnotna))
    candidates = uxnotna[tab == max(tab)]
    if (class(x)[1]  == "logical") {
      any(candidates) # return TRUE if any true. max returns an integer
    } else {
      max(candidates) # return highest (ie max) value
    }
  } else {
    ux   # this returns the NA with the right class. ie that of x
  }
}
