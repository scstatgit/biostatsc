#' @export
mytests <- function(varvalue, varname, groupvalue, groupname){
  if(is.numeric(varvalue)){
    if((normality_test(ddd, varname, groupname) == TRUE) & nlevels(groupvalue)==2){
      res <- t_test(ddd, varname, groupname)
      method <- "T-test"
    } else if(nlevels(groupvalue)>=3) {
      res <- aov_test(ddd, varname, groupname)
      method <- "ANOVA"
    } else if(nlevels(groupvalue)==2) {
      res <- wilcox_test(ddd, varname, groupname)
      method <- "Wilcoxon rank sum test"
    } else if(nlevels(groupvalue)>=3) {
      res <- kruskal_test(ddd, varname, groupname)
      method <- "Kruskal-Wallis test"
    } else {
      res <- NULL
      method <- ""
    }
    data.frame(Characteristics = varname, pval = res, method = method)
  } else if(is.factor(varvalue)|is.character(varvalue)){
    if(chisq_assumption(ddd, varname, groupname) == TRUE){
      res <- chisq_test(ddd, varname, groupname)$p.value
      method <- chisq_test(ddd, varname, groupname)$method
    } else {
      res <- fisher_test(ddd, varname, groupname)
      method <- "Fisher's exact test"
    }
    data.frame(Characteristics = varname, pval = res, method = method)
  }
}
