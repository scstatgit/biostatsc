#' table1 function
#'
#' This function create table1 in biostatistics
#' @param DATA: put data frame
#' @param STRATA: put stratification varaible
#' @param OVERALL: if you want overall statistics, set TRUE
#' @param CONDIGITS: digits of continuous variable
#' @param CATDIGITS: digits of categorical variable
#' @param FUNC: "mean" or "median"
#' @param TYPE: type of print FUNC
#' @param MARGIN: margin of percentage. 1 as row percent, and 2 as col percent
#' @keywords create table1 in biostatistics
#' @examples
#' ddd <- mtcars
#' ddd <- preprocess(ddd)
#' TAB1(ddd)
#' TAB1(ddd, STRATA = "cyl")
#' TAB1(ddd, STRATA = "cyl", OVERALL = TRUE)
#' TAB1(ddd, STRATA = "cyl", OVERALL = TRUE, CATDIGITS = 0)
#' TAB1(ddd, STRATA = "cyl", OVERALL = TRUE, CONDIGITS = 3, CATDIGITS = 0, FUNC = "median", TYPE = 1, MARGIN = 1)
#' @export
TAB1 <- function(DATA, STRATA = NULL, OVERALL = FALSE, CONDIGITS = 1, CATDIGITS = 1, FUNC = "mean", TYPE = 1, MARGIN = 2){
  mat1 <- descirbe0(DATA, var.lvs)
  mat2 <- describe1(DATA)
  if(is.null(STRATA)){
    res <- cbind(mat1,mat2)
  }
  if(!is.null(STRATA)){
    mat3 <- describe2(DATA, STRATA = STRATA,CONDIGITS=CONDIGITS,CATDIGITS=CATDIGITS,FUNC=FUNC,TYPE=TYPE,MARGIN=MARGIN)
    res <- cbind(mat1,mat3)
  }
  if(!is.null(STRATA) & isTRUE(OVERALL)){
    mat3 <- describe2(DATA, STRATA = STRATA,CONDIGITS=CONDIGITS,CATDIGITS=CATDIGITS,FUNC=FUNC,TYPE=TYPE,MARGIN=MARGIN)
    res <- cbind(mat1,mat2,mat3)
  }
  if(!is.null(STRATA)){
    www <- which(res[,1] == STRATA)
    res <- res[-www,]
  }
  return(res)
}


