#' @export
i <- function(if_stat, then) {
  if_stat <- lazyeval::expr_text(if_stat)
  then    <- lazyeval::expr_text(then)
  sprintf("ifelse(%s, %s, ", if_stat, then)
}
#' @export
e <- function(else_ret) {
  else_ret <- lazyeval::expr_text(else_ret)
  else_ret
}
#' nested_if_else() Function
#'
#' source: https://www.r-bloggers.com/a-wrapper-around-nested-ifelse/
#' This function find formula from string
#' @param if_stat, then
#' @examples
#' ie(
#'   i(DATA$EDUC %in% c(1,2), 1),
#'   i(DATA$EDUC==3, 2),
#'   i(DATA$EDUC==4, 3),
#'   e(888)
#' )
#' @keywords nested_if_else
#' @export
ie <- function(...) {
  args <- list(...)

  for (i in 1:(length(args) - 1) ) {
    if (substr(args[[i]], 1, 6) != "ifelse") {
      stop("All but the last argument, need to be i functions.", call. = FALSE)
    }
  }
  if (substr(args[[length(args)]], 1, 6) == "ifelse"){
    stop("Last argument needs to be an e function.", call. = FALSE)
  }
  args$final <- paste(rep(')', length(args) - 1), collapse = '')
  eval_string <- do.call('paste', args)
  eval(parse(text = eval_string))
}
