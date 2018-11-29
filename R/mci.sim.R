mci.sim <-
function (mcimodel, origins.pot, ..., mcitrans = "lc", interc = NULL) {
  
  mcidataset <- mcimodel$mcimat
  
  names(mcidataset)[names(mcidataset) == "p_ij"] <- "p_ij_emp"
  
  cat ("\n")
  cat ("Multiplicative Competitive Interaction Model (MCI)", "\n")
  cat (paste0("Function link: ", mcitrans), "\n")
  cat ("\n")
  
  mcimat_new <- mci.shares (mcidataset, "i_orig", "j_dest", ...,  mcitrans = mcitrans, interc)
  
  mcitotal <- shares.total(mcidataset = mcimat_new, 
                           submarkets = "i_orig", 
                           suppliers = "j_dest", 
                           shares = "p_ij", 
                           localmarket = origins.pot, 
                           check_df = FALSE)

  colnames(mcitotal) <- c("j_dest", "T_j", "T_j_share")

  cat("Total results", "\n")
  print(as.data.frame(mcitotal))
  cat("\n")
  
  invisible (list (mcimat = mcimat_new, mcitotal = mcitotal))
  
}