# https://www.r-bloggers.com/a-wrapper-around-nested-ifelse/
i <- function(if_stat, then) {
  if_stat <- lazyeval::expr_text(if_stat)
  then    <- lazyeval::expr_text(then)
  sprintf("ifelse(%s, %s, ", if_stat, then)
}
e <- function(else_ret) {
  else_ret <- lazyeval::expr_text(else_ret)
  else_ret
}
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