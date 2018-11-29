huff.newdest <-
function (huffmodel, newdest.id, newdest.addr, newdest.attract, newdestaddr.format = "stradr", distval = NULL) {
  

  
  if ((length (newdest.id) > 1) | (length (newdest.addr) > 1) | (length (newdest.attract) > 1)) {
    stop("One new destination must be specified")
  }
  
  tc_mode <- huffmodel$tc.mode
  

  
  huffmat_add <- data.frame(matrix (ncol = 9, nrow = nrow(huffmodel$coords$coords_origins)))

  
  
  if (is.null(distval)) {


    
  newtc <- tcmat.create(origins.id = huffmodel$coords$coords_origins$origins.id,
                        origins.addr = huffmodel$coords$coords_origins$origins.addr,
                        destinations.id = newdest.id,
                        destinations.addr = newdest.addr,
                        tc.type = tc_mode$tc.type, tc.unit = tc_mode$tc.unit, 
                        addr.format = newdestaddr.format, tc.constant = tc_mode$tc.constant)
  

  huffmat_add[,1] <- newtc$tcmat$from_to
  huffmat_add[,2] <- newtc$tcmat$from
  huffmat_add[,3] <- huffmodel$coords$coords_origins$origins.pot
  huffmat_add[,4] <- newtc$tcmat$to
  huffmat_add[,5] <- newdest.attract
  huffmat_add[,6] <- newtc$tcmat$tc
  huffmat_add[,7:9] <- NA
  
  }
  else {

    if (nrow(huffmodel$coords$coords_origins) != length(distval)) {
      stop("Origins and transport costs data do not match")
    }
    
    huffmat_add[,2] <- huffmodel$coords$coords_origins$origins.id
    huffmat_add[,3] <- huffmodel$coords$coords_origins$origins.pot
    huffmat_add[,4] <- newdest.id
    huffmat_add[,5] <- newdest.attract
    huffmat_add[,6] <- distval
    huffmat_add[,1] <- paste0(huffmat_add[,2], "-", huffmat_add[,4])
    huffmat_add[,7:9] <- NA

  }
  

  
  colnames(huffmat_add) <- c("ij", "i_orig", "C_i", "j_dest", "A_j", "d_ij", "U_ij", "sum_U_ij", "p_ij")
  

  
  huffmat_update <- rbind(huffmodel$huffmat, huffmat_add)
  

  
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
  cat(paste0("Destination added: ", newdest.id, "\n"))
  cat("\n")
  cat("Total results", "\n")
  print(as.data.frame(hufftotal))
  cat("\n")
  cat(paste0("Attraction min.: ", min(huffmat$A_j), ", max.: ", max(huffmat$A_j), ", mean: ", round(mean(huffmat$A_j),2), "\n"))
  cat(paste0("Transport cost min.: ", round(min(huffmat$d_ij),2), ", max.: ", round(max(huffmat$d_ij),2), ", mean: ", round(mean(huffmat$d_ij),2), "\n"))
  cat("\n")

  coords_newdest <- data.frame(c(newtc$coords_destinations[1], newdest.attract, newtc$coords_destinations[2:4]))
  colnames(coords_newdest) <- c("destinations.id", "destinations.attrac", "destinations.addr", "destinations.x_lon", "destinations.y_lat")
  
  
  invisible (list (huffmat = huffmat, 
                   hufftotal = hufftotal, 
                   params = huffmodel$params, 
                   coords = list(coords_origins = huffmodel$coords$coords_origins, 
                                 coords_destinations = huffmodel$coords$coords_destinations,
                                 coords_newdest = coords_newdest),
                   tc_mode = tc_mode))
  
}
