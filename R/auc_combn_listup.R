#' auc_combn_listup Function
#'
#' This function can list-up AUC from multiple combinations.
#' @param DATA: put data frame
#' @param DV: put Dependent Variable like this way: c("_DV_")
#' @param IV: put Independent Variable like this way: c("_IV1_", "_IV2_", ...)
#' @param IV.REF: choose one of Reference Independent Varaible from IV
#' @param COMBN_VAL: consider how many combinations you want: 1, 2, 3, ...
#' @param ALGORITHM: you can choose "+" for multiplicity or "*" for interactions
#' @param DISPLAY.NEVENT: If TRUE then print EVENT/N, default is FALSE.
#' @param DISPLAY.EQUATION: If TRUE then print equation, default is FALSE.
#' @param SEED: set.seed, default is 123. for calculating bootstrap CI.
#' @keywords AUC Combination Follow-up
#' @examples
#' auc_combn_listup(mtcars,"vs",c("mpg","cyl","disp","hp","drat"),"mpg",1)
#' auc_combn_listup(mtcars,"vs",c("mpg","cyl","disp","hp","drat"),"mpg",2)
#' auc_combn_listup(mtcars,"vs",c("mpg","cyl","disp","hp","drat"),"mpg",3)
#' @export
auc_combn_listup <- function(DATA, DV, IV, IV.REF, COMBN_VAL=1, ALGORITHM="+", DISPLAY.NEVENT=FALSE, DISPLAY.EQUATION=FALSE, SEED=123){
  if(!isBinary(DATA[[DV]])) {
    warning("DV must be binary data")
    break
  }
  tmp <- DATA
  tmp[[DV]] <- factor(tmp[[DV]], exclude = T)
  tag.healthy <- levels(tmp[[DV]])[1]
  tag.disease <- levels(tmp[[DV]])[2]
  combinations <- as.data.frame(combn(IV,COMBN_VAL))
  vars <- sapply(combinations, function(x) paste(x, collapse = paste0(" ",ALGORITHM," ")))

  set.seed(SEED)
  res <- c()
  for (var in vars){
    var.xx <- strsplit(var, split = paste0(" ",ALGORITHM," "), fixed = TRUE)[[1]]
    tmpp0 <- subset(tmp, select=c(DV,IV.REF)) %>% as.data.frame()
    tmpp0 <- na.omit(tmpp0)
    tmpp <- subset(tmp, select=c(DV,var.xx)) %>% as.data.frame()
    tmpp <- na.omit(tmpp)

    mf <- as.formula(paste(DV, ".", sep=" ~ "))
    ff <- glm(mf, family = binomial, data = tmpp)
    tmpp["predictor"] <- predict(ff, type="respons")

    ROC0 <- roc_(tmpp0, DV, IV.REF) %>% suppressMessages()
    ROC1 <- roc_(tmpp, DV, "predictor") %>% suppressMessages()
    direction <- ROC1$direction
    coff <- OptimalCutpoints::optimal.cutpoints("predictor", DV, tag.healthy = tag.healthy, methods = "Youden", data = tmpp, direction=direction)$Youden$Global$optimal.cutoff$cutoff[1]
    if(auc(ROC1)<0.5){
      direction <- ifelse(ROC1$direction=="<","\u02C3","\u02C2")
      ROC1 <- roc_(tmpp, DV, "predictor", direction=direction) %>% suppressMessages()
      coff <- OptimalCutpoints::optimal.cutpoints(var.xx, DV, tag.healthy = tag.healthy, methods = "Youden", data = tmpp, direction=direction)$Youden$Global$optimal.cutoff$cutoff[1]
    }
    coff <- sprintf("%.3f", coff); direction <- ifelse(ROC1$direction=="<","\u2265","\u2264")
    coff <- paste0(direction,coff)
    pval <- roc.test(ROC0, ROC1)[["p.value"]]
    pval <- ifelse(pval<0.001,"<0.001",round(pval,3))

    ROC <- ROC1
    ccc <- coords(ROC, x="best", input="threshold", best.method="youden", ret=c("threshold", "sensitivity", "specificity", "tp", "tn", "fp", "fn"));
    TP <- ccc$tp[1]
    TN <- ccc$tn[1]
    FP <- ccc$fp[1]
    FN <- ccc$fn[1]

    ccc2 <- ci.coords(ROC, ccc$threshold)
    sens <- sprintf("%.3f (%.3f, %.3f)", ccc2$sensitivity[2], ccc2$sensitivity[1], ccc2$sensitivity[3])
    spec <- sprintf("%.3f (%.3f, %.3f)", ccc2$specificity[2], ccc2$specificity[1], ccc2$specificity[3])
    auc <- sprintf("%.3f (%.3f, %.3f)", ci.auc(ROC)[2], ci.auc(ROC)[1], ci.auc(ROC)[3])
    tab <- table(tmpp[[DV]])
    prop <- sprintf("%.1f%%", prop.table(tab)[2]*100)
    gname <- paste0(tag.disease," vs. ",tag.healthy, " (ref.)")
    tot.n <- sum(tab)
    tot.event <- tab[2]
    row <- data.frame(Outcome = gname, Variables = var, AUC = auc, Sens = sens, Spec = spec, Cutoff = coff, TP = TP, TN = TN, FP = FP, FN = FN, pval = pval)
    if(DISPLAY.NEVENT==TRUE){
      row["N"] <- tot.n
      row["Event"] <- tot.event
      row["Percent"] <- prop
    }
    if(DISPLAY.EQUATION==TRUE){
      row["Equations"] <- getEquations(ff)
    }
    res <- rbind(res, row)
    row.names(res) <- NULL
  }
  return(res)
}
