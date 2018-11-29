huff.updest <-
function (huffmodel, dest.id, dest.attrac) {
  
  if (dest.attrac < 0) {

    stop("Attraction value must equal to or greater than zero")
  }
  

  huffmat_update <- huffmodel$huffmat
  
  huffmat_update[huffmat_update$j_dest == dest.id,]$A_j <- dest.attrac

  huffmodel$coords$coords_destinations[huffmodel$coords$coords_destinations$destinations.id == dest.id,]$destinations.attrac <- dest.attrac

  huffmat <- huff.shares(huffdataset = huffmat_update, origins = "i_orig", 
                         locations = "j_dest", attrac = "A_j", dist = "d_ij",
                         gamma = huffmodel$params[1,1], lambda = huffmodel$params[2,1], 
                         atype = rownames(huffmodel$params)[1], dtype = rownames(huffmodel$params)[2], 
                         gamma2 = huffmodel$params[1,2], lambda2 = huffmodel$params[2,2],
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
  cat(paste0("Attraction function: ", rownames(huffmodel$params)[1]), "\n")
  cat(paste0("Transport costs function: ", rownames(huffmodel$params)[2]), "\n")
  cat(paste0("Destination updated: ", dest.id, "\n"))
  cat("\n")
  cat("Total results", "\n")
  print(as.data.frame(hufftotal))
  cat("\n")
  cat(paste0("Attraction min.: ", min(huffmat$A_j), ", max.: ", max(huffmat$A_j), ", mean: ", round(mean(huffmat$A_j),2), "\n"))
  cat(paste0("Transport cost min.: ", round(min(huffmat$d_ij),2), ", max.: ", round(max(huffmat$d_ij),2), ", mean: ", round(mean(huffmat$d_ij),2), "\n"))
  cat("\n")
  
  
  invisible (list (huffmat = huffmat, 
                   hufftotal = hufftotal, 
                   params = huffmodel$params, 
                   coords = list(coords_origins = huffmodel$coords$coords_origins, 
                                 coords_destinations = huffmodel$coords$coords_destinations),
                   tc_mode = huffmodel$tc_mode))
  
}
