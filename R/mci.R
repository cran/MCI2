mci <-
function (mcimat, shares, ..., 
                 no.intercept = TRUE, mci.weighting = FALSE, mci.weights = NULL, 
                 show_proc = FALSE) {
  
  if (mci.weighting == TRUE) {
    mci.method <- "WLS"
  }
  else {
    mci.method <- "OLS"
  }
  
  cat ("\n")
  cat ("Multiplicative Competitive Interaction Model (MCI)", "\n")
  cat (paste0("Estimation: ", mci.method), "\n")
  cat ("\n")
  
  mcidataset <- mcimat$mcimat
  
  mcimat_t <- mci.transmat (mcidataset, "i_orig", "j_dest", shares, ..., show_proc = FALSE)


  mcimat_t_columns <- ncol(mcimat_t) 
  mcimat_t_rows <- nrow(mcimat_t) 
  mcimat_t_colnames <- colnames(mcimat_t) 
  mci_depvar <- mcimat_t_colnames[3] 

  mci_expvars <- mcimat_t_colnames[4:mcimat_t_columns] 

  mci_expvars_formula <- paste(mci_expvars, collapse=" + ") 

  
  if (no.intercept == TRUE) { 
    mci_formula <- paste(mci_depvar, "~ 0 +", mci_expvars_formula) 
    } 
  else { 
    mci_formula <- paste(mci_depvar, "~", mci_expvars_formula) 
  } 
  
  
  if (mci.weighting == TRUE) {
  
    if (is.null(mci.weights)) {

      mcimodel_noweighting <- lm (mci_formula, data = mcimat_t)
      mcimat_t$mci.weights <- 1/(mcimodel_noweighting$residuals^2)
    }
    else {
    mcimat_t$mci.weights <- mci.weights

    }
    
    mcimodel <- lm (mci_formula, data = mcimat_t, weights = mci.weights)


  }
  
  else {
    mcimodel <- lm (mci_formula, data = mcimat_t) 
  }
  
  mcimodel_summary <- summary(mcimodel)

  mcimodel_coef <- mcimodel_summary$coefficients
  
  cat ((paste0("Model coefficients", "\n")))
  print(as.data.frame(mcimodel_coef))
  
  
  mcimodel_stat <- matrix (nrow = 2, ncol = 5)
  colnames(mcimodel_stat) <- c("Estimate", "F value", "df 1", "df 2", "Pr (>F)")
  rownames(mcimodel_stat) <- c("R-squared", "Adj. R-squared")
  
  mcimodel_stat[1,1] <- mcimodel_summary$r.squared

  mcimodel_stat[2,1] <- mcimodel_summary$adj.r.squared

  
  mcimodel_stat[1, 2:4] <- mcimodel_summary$fstatistic[1:3]
  
  mcimodel_stat[1,5] <- pf(mcimodel_stat[1,2], mcimodel_stat[1,3], mcimodel_stat[1,4], lower.tail = FALSE) 

  
  cat ("Model summary", "\n")
  print(as.data.frame(mcimodel_stat))
  cat("\n")
  
  invisible(list (mcimat = mcidataset, regdata = mcimat_t, mcimodel_coef = mcimodel_coef, mcimodel_stat = mcimodel_stat))
  
}
