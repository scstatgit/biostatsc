#' strsplit_f() Function
#'
#' This function find formula from string
#' @param string, find: find formula from string
#' @keywords strsplit_f
#' @examples
#' strsplit_f()
#'
#' @export
strsplit_f <- function(string, find){
  mapply(function(x, y) stri_detect_regex(x, paste0('^.*',y,'((?!/).)*$')), x=string, y=find)
}
