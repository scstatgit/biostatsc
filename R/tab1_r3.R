#' tab1_r3 function
#'
#' This function create table1 in biostatistics for repeated measures, v3: by time: count n(id)
#' @param DATA: put data frame
#' @param ID: put id varaible
#' @param TIME: put repeated measures varaible (for stratified)
#' @param FUNC: "mean" or "median"
#' @param CONDIGITS: digits of continuous variable
#' @param CATDIGITS: digits of categorical variable
#' @param TYPE: type of print FUNC
#' @keywords create table1 in biostatistics for repeated measures data
#' @examples
#' ddl <- pbcseq_modified
#' # baseline: count n(id)
#' tab1_r1(ddl, "id", "day")
#' # by overall: count n(case)
#' tab1_r2(ddl, "id", "day")
#' # by time: count n(id)
#' tab1_r3(ddl, "id", "day", "day.g")
#' @export
tab1_r3 <- function(DATA, ID, TIME, GROUP, FUNC = "mean", CONDIGITS = 1, CATDIGITS = 1, TYPE = 1){
  if(FUNC=="mean"){f_conti <- function(x) var_msd(var=x, digits = CONDIGITS, type = TYPE)}
  if(FUNC=="median"){f_conti <- function(x) var_miqr(var=x, digits = CONDIGITS, type = TYPE)}
  res <- DATA %>%
    arrange_(ID, TIME) %>%
    group_by_(GROUP) %>%
    .[, !(colnames(.) %in% c(ID, TIME))] %>%
    tibble::add_column(n=factor("Overall"), .before=1) %>%
    summarise_all(~if(is.numeric(.)) f_conti(.) else Freq(., CATDIGITS)) %>%
    t()
  return(res)
}
