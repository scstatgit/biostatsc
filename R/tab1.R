#' table1 function
#'
#' This function can follows-up AUC from multiple combinations, where P-value < 0.05 for Delong's test.
#' @param DATA: put data frame
#' @param STRATA: put stratification varaible
#' @param OVERALL: if you want overall statistics, set TRUE
#' @param CONDIGITS: digits of continuous variable
#' @param CATDIGITS: digits of categorical variable
#' @param FUNC: "mean" or "median"
#' @param TYPE: type of print FUNC
#' @param MARGIN: margin of percentage. 1 as col percent, and 2 as row percent
#' @keywords Automatically create table1 in biostatistics
#' @examples
#' ddd <- mtcars
#' ddd <- preprocess(ddd)
#' TAB1(ddd)
#' TAB1(ddd, STRATA = "cyl")
#' TAB1(ddd, STRATA = "cyl", OVERALL = TRUE)
#' TAB1(ddd, STRATA = "cyl", OVERALL = TRUE, CATDIGITS = 0)
#' TAB1(ddd, STRATA = "cyl", OVERALL = TRUE, CONDIGITS = 3, CATDIGITS = 0, FUNC = "median", TYPE = 1, MARGIN = 1)
#' @export
msd <- function(var,digits=3,type=1){
  if(type==1){
    res <- sprintf(paste0("%.",digits,"f"," ± %.",digits,"f"), mean(var, na.rm=TRUE), sd(var, na.rm=TRUE))
  }
  if(type==2){
    res <- sprintf(paste0("%.",digits,"f"," (%.",digits,"f)"), mean(var, na.rm=TRUE), sd(var, na.rm=TRUE))
  }
  return(res)
}
#' @export
miqr <- function(var,digits=3,type=1){
  quant <- quantile(var, na.rm=TRUE)
  if(type==1){
    res <- sprintf(paste0("%.",digits,"f"," [%.",digits,"f, %.",digits,"f]"), quant[3], quant[2], quant[4])
  }
  if(type==2){
    res <- sprintf(paste0("%.",digits,"f - %.",digits,"f"), quant[1], quant[5])
  }
  if(type==3){
    res <- sprintf(paste0("Median: %.",digits,"f\n","[Q1: %.",digits,"f, Q3: %.",digits,"f]\n",
                          "Range: %.",digits,"f - %.",digits,"f"), quant[3], quant[2], quant[4], quant[1], quant[5])
  }
  return(res)
}
#' @export
nperc <- function(DATA, var1, var2, digits=0, type=0){
  DATA <- DATA
  xt <- table(DATA[[var1]], DATA[[var2]])
  pt0 <- prop.table(xt, margin=NULL)
  pt1 <- prop.table(xt, margin=1) # row
  pt2 <- prop.table(xt, margin=2) # col
  if(type==0){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, pt0*100)
  }
  if(type==1){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, t(pt1)*100)
  }
  if(type==2){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, t(pt2)*100)
  }
  return(res)
}
#' @export
nperc1 <- function(var, digits=0){
  xt <- table(var)
  pt0 <- prop.table(xt)
  res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, pt0*100)
  return(res)
}
#' @export
nperc2 <- function(var, digits=0, type=0){
  xt <- table(var)
  pt0 <- prop.table(xt, margin=NULL)
  pt1 <- prop.table(xt, margin=1) # row
  pt2 <- prop.table(xt, margin=2) # col
  if(type==0){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, pt0*100)
  }
  if(type==1){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, pt1*100)
  }
  if(type==2){
    res <- sprintf(paste0("%.f (%.",digits,"f%%)"), xt, pt2*100)
  }
  return(res)
}
#' @export
var.nlevels <- function(DATA, var){
  vv <- var
  vv.nlevels <- plyr::count(vv)
  res <- length(vv.nlevels$x)
  return(res)
}
#' @export
var.levels <- function(DATA, var){
  vv <- var
  vv.nlevels <- plyr::count(DATA[[vv]])
  res <- vv.nlevels$x
  return(res)
}
# automatically convert numeric to factor, if var.lvs <= 5
#' @export
varlvs <- function(DATA){
  res <- sapply(DATA, function(var) var.nlevels(DATA,var))
  return(res)
}
#' @export
autofac <- function(DATA){
  for(var in names(var.lvs)){
    if(var.lvs[[var]]<=5){
      DATA[[var]] <- as.factor(DATA[[var]])
    }
  }
  return(DATA)
}
#' @export
varfac <- function(DATA){
  res <- sapply(DATA, is.factor)[sapply(DATA, is.factor)] %>% names()
  return(res)
}
#' @export
varnum <- function(DATA){
  res <- sapply(DATA, is.numeric)[sapply(DATA, is.numeric)] %>% names()
  return(res)
}
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
# Descriptive statistics for overall
#' @export
describe1 <- function(DATA,CONDIGITS=1,CATDIGITS=1,FUNC="mean",TYPE=1){
  vals <- c()
  for(var in names(DATA)){
    if(var %in% var.num){
      if(FUNC=="mean"){ val <- DATA[[var]] %>% msd(digits = CONDIGITS, type = TYPE) }
      if(FUNC=="median"){ val <- DATA[[var]] %>% miqr(digits = CONDIGITS, type = TYPE) }
    }
    if(var %in% var.fac){
      val <- DATA[[var]] %>% nperc1(digits = CATDIGITS)
    }
    vals <- c(vals, val)
  }
  mat <- matrix(vals, ncol=1)
  colnames(mat) <- "overall"
  return(mat)
}
# Descriptive statistics by strata
#' @export
describe2 <- function(DATA,STRATA,CONDIGITS=1,CATDIGITS=1,FUNC="mean",TYPE=1,MARGIN=2){
  vals <- c()
  for(var in names(DATA)){
    if(var %in% var.num){
      if(FUNC=="mean"){ val <- tapply(DATA[[var]], INDEX = DATA[[STRATA]], function(var) msd(var, digits = CONDIGITS, type = TYPE)) }
      if(FUNC=="median"){ val <- tapply(DATA[[var]], INDEX = DATA[[STRATA]], function(var) miqr(var, digits = CONDIGITS, type = TYPE)) }
    }
    if(var %in% var.fac){
      vec <- nperc(DATA, var, STRATA, type = MARGIN, digits = CATDIGITS)
      val <- matrix(vec, ncol=var.lvs[[STRATA]])
    }
    vals <- c(vals, val)
  }
  mat <- matrix(vals, ncol=var.lvs[[STRATA]], byrow=T)
  colnames(mat) <- as.character(var.levels(ddd,STRATA))
  return(mat)
}
#' @export
preprocess <- function(DATA){
  var.lvs <<- varlvs(DATA)
  DATA <- autofac(DATA)
  var.fac <<- varfac(DATA)
  var.num <<- varnum(DATA)
  return(DATA)
}
#' @export
TAB1 <- function(DATA, STRATA=NULL, OVERALL=FALSE, CONDIGITS=1,CATDIGITS=1,FUNC="mean",TYPE=1,MARGIN=2){
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
  return(res)
}


