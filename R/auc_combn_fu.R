#' auc_combn_fu Function
#'
#' This function can follows-up AUC from multiple combinations, where P-value < 0.05 for Delong's test.
#' @param DATA: put data frame
#' @param DV: put Dependent Variable like this way: c("_DV_")
#' @param IV: put Independent Variable like this way: c("_IV1_", "_IV2_", ...)
#' @param COMBN_VAL: consider how many combinations you want: 1, 2, 3, ...
#' @param ALGORITHM: you can choose "+" for multiplicity or "*" for interactions
#' @keywords AUC Combination Follow-up
#' @examples
#' auc_combn_fu(mtcars,c("vs"),c("mpg","cyl","disp","hp","drat"),5)
#' auc_combn_fu(mtcars,c("vs"),c("mpg","cyl","disp","drat"),4)
#' auc_combn_fu(mtcars,c("vs"),c("mpg","cyl","disp","hp","drat","wt","qsec","am","gear","carb"),3)
#' @export
auc_combn_fu <- function(DATA, DV, IV, COMBN_VAL, ALGORITHM = "+"){
  nm <- make.names(names(DATA), unique = T, allow_ = T)
  TMP <- subset(DATA, select = c(DV,IV))
  IVX <- paste0("X",1:length(IV))
  names(TMP) <- c("Y",IVX)

  out <- c(); rocs <- c(); cc <- 0;
  for(combn_val in 1:COMBN_VAL){

    var_list = combn(IVX, combn_val); var_list.lab = combn(IV, combn_val)

    for(ii in 1:ncol(var_list)){

      kk <- var_list[,ii]
      dd <- subset(TMP, select = c("Y", kk)) %>%
        na.omit()

      ## original data
      ff <- formula(paste("Y ~", paste0(kk, collapse = ALGORITHM)))
      fit <- glm(ff, data=dd, family = binomial(link="logit")); summary(fit)
      rocobj <- roc(dd[["Y"]] ~ fitted(fit, type), ci=T)
      auc <- rocobj[["ci"]][2]; auc_ci = sprintf("(%.3f - %.3f)", rocobj[["ci"]][1], rocobj[["ci"]][3])

      rst <- data.frame(
        combn_val = combn_val,
        variable = paste(var_list.lab[,ii], collapse = paste0(",")),
        n = nrow(dd),
        auc = auc,
        auc_ci = auc_ci)

      out <- rbind(out, rst)
      cc <- cc+1
      rocs[[cc]] <- rocobj

    }
  }
  res <- out %>% as.data.frame

  delong_tests <- function(combn_val_i){
    if(combn_val_i==0){
      www <- which(res$combn_val==1)
    }
    if(combn_val_i==1){
      www <- which(res$combn_val==1)
    }
    if(combn_val_i>=2){
      www <- sigs %>% filter(combn_val==combn_val_i) %>% select(ord) %>% as.matrix %>% c()
    }
    if(!is.logical(www)){
      for(jj in 1:length(www)){
        if(combn_val_i==0){
          pvals <- c()
          for(ii in 1:length(rocs)){
            pvals <- c(pvals, wilcox.test(rocs[[www[jj]]]$cases,rocs[[www[jj]]]$controls)$p.value)
          }
          newd <- data.frame(ref=res[www,"variable"][jj],res,pval=pvals)
          result <- newd %>%
            mutate(ord=seq_along(n)) %>%
            select(ord, combn_val, ref, variable, n, auc, auc_ci, pval) %>%
            filter(variable == ref) %>%
            filter(combn_val==combn_val_i+1)
        }
        if(combn_val_i==1){
          pvals <- c()
          for(ii in 1:length(rocs)){
            pvals <- c(pvals, delong_test(rocs[[www[jj]]],rocs[[ii]]))
          }
          newd <- data.frame(ref=res[www,"variable"][jj],res,pval=pvals)
          result <- newd %>%
            mutate(ord=seq_along(n)) %>%
            select(ord, combn_val, ref, variable, n, auc, auc_ci, pval) %>%
            filter(strsplit_f(variable, ref)) %>%
            filter(pval<0.05 & combn_val==combn_val_i+1)
        }
        if(combn_val_i>=2){
          pvals <- c()
          for(ii in 1:length(rocs)){
            pvals <- c(pvals, delong_test(rocs[[www[jj]]],rocs[[ii]]))
          }
          newd <- data.frame(ref=res[www,"variable"][jj],res,pval=pvals)
          result <- newd %>%
            mutate(ord=seq_along(n)) %>%
            select(ord, combn_val, ref, variable, n, auc, auc_ci, pval) %>%
            filter(strsplit_f(variable, ref)) %>%
            filter(pval<0.05 & combn_val==combn_val_i+1)
        }
        sigs <- rbind(sigs,result)
      }
      return(sigs)
    } else {
      return(delong_tests(combn_val_i-1))
    }
  }
  sigs <- c()
  for(combn_val in 0:(COMBN_VAL-1)){
    sigs <- delong_tests(combn_val)
  }

  FU <- function(delongs, combn_val_i){
    if(combn_val_i==1){
      x <- delongs
      x <- subset(x, combn_val==1)
      x[["key"]] <- x[["variable"]]
      res <- x[,(names(x) %in% c("key","variable","n","auc","auc_ci","pval"))]
      return(res)
    }
    if(combn_val_i>=2){
      x <- FU(sigs,combn_val_i-1)
      if(combn_val_i>=3){
        x[["key"]] <- x[[paste0("variable_",combn_val_i-1)]]
        x[[paste0("variable_",combn_val_i-1)]] <- gsub(",",paste0(" ",ALGORITHM," "),x[[paste0("variable_",combn_val_i-1)]])
      }
      y <- delongs
      y <- subset(y, combn_val==combn_val_i)
      y[["key"]] <- y[["ref"]]
      y <- y[,!(names(y) %in% c("ord","ref","combn_val"))]
      res <- left_join(x, y, by = "key", suffix = c("",paste0("_",combn_val_i)))
      res <- res[,!(names(res) %in% c("key"))]
      return(res)
    }
  }
  final <- c()
  for(combn_val in 1:(COMBN_VAL)){
    final <- FU(sigs,combn_val)
  }
  final[[paste0("variable_",combn_val)]] <- gsub(",",paste0(" ",ALGORITHM," "),final[[paste0("variable_",combn_val)]])
  return(final)
}
