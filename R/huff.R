huff <-
function (origins.id, origins.pot, 
                  destinations.id, destinations.attrac, 
                  tcmat, 
                  atype = "pow", gamma = 1, gamma2 = NULL,
                  dtype = "pow", lambda = -2, lambda2 = NULL) {
  
  checkod(origins.id, destinations.id)

  tcmat_fromto <- tcmat$tcmat[,3:4]

  origins <- data.frame(origins.id, origins.pot)

  
  destinations <- data.frame(destinations.id, destinations.attrac)

  
  od_mat <- merge(origins, destinations)

  od_mat$od <- paste0(od_mat$origins.id, "-", od_mat$destinations.id)

  
  od_mat_dist <- merge (od_mat, tcmat_fromto, by.x = "od", by.y = "from_to", all.x = TRUE)

  

  
  huffmat <- huff.shares(huffdataset = od_mat_dist, origins = "origins.id", 
                         locations = "destinations.id", attrac = "destinations.attrac", dist = "tc",
                         gamma = gamma, lambda = lambda, atype = atype, dtype = dtype, gamma2 = gamma2, lambda2 = lambda2,
                         check_df = FALSE)

  colnames(huffmat) <- c("ij", "i_orig", "C_i", "j_dest", "A_j", "d_ij", "U_ij", "sum_U_ij", "p_ij")
  

  hufftotal <- shares.total(mcidataset = huffmat, 
                            submarkets = "i_orig", 
                            suppliers = "j_dest", 
                            shares = "p_ij", 
                            localmarket = "C_i", 
                            check_df = FALSE)

  
  colnames(hufftotal) <- c("j_dest", "T_j", "T_j_share")

  
  
  cat("\n")
  cat("Huff Model", "\n")
  cat(paste0("Attraction function: ", atype), "\n")
  cat(paste0("Transport costs function: ", dtype), "\n")
  cat("\n")
  cat("Total results", "\n")
  print(as.data.frame(hufftotal))
  cat("\n")
  cat(paste0("Attraction min.: ", min(huffmat$A_j), ", max.: ", max(huffmat$A_j), ", mean: ", round(mean(huffmat$A_j),2), "\n"))
  cat(paste0("Transport cost min.: ", round(min(huffmat$d_ij),2), ", max.: ", round(max(huffmat$d_ij),2), ", mean: ", round(mean(huffmat$d_ij),2), "\n"))
  cat("\n")
  
  

  coords_origins <- merge (origins, tcmat$coords_origins, by.x = "origins.id", by.y = "origins.id")


  coords_destinations <- merge (destinations, tcmat$coords_destinations, by.x = "destinations.id", by.y = "destinations.id")
  coords <- list (coords_origins = coords_origins, coords_destinations = coords_destinations)


  tc.mode <- tcmat$tc.mode
  

  params <- matrix (ncol = 2, nrow = 2)
  params[1,1] <- gamma
  if (!is.null(gamma2)) { params[1,2] <- gamma2 }
  else { params[1,2] <- NA }
  params[2,1] <- lambda
  if (!is.null(lambda2)) { params[2,2] <- lambda2 }
  else { params[2,2] <- NA}
  rownames(params) <- c(atype, dtype)
  colnames(params) <- c("param1", "param2")

  
  invisible (list (huffmat = huffmat, hufftotal = hufftotal, params = params, coords = coords, tc.mode = tc.mode))

   
}
