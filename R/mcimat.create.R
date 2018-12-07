mcimat.create <-
function (rawdata, origins.id, destinations.id, ..., 
                           tcmat,
                           origvar.data = NULL, origvardata.id = NULL,
                           destvar.data = NULL, destvardata.id = NULL,
                           remOrig = NULL, remDest = NULL,
                           corObserved = 0,
                           remNA = TRUE) {
  
  checkod(origins.id, destinations.id)
  
  if (!is.null(remOrig)) {

    rawdata <- subset (rawdata, !(rawdata[[origins.id]] %in% remOrig))
  }
  
  if (!is.null(remDest)) {
    rawdata <- subset (rawdata, !(rawdata[[destinations.id]] %in% remDest))
  }
  
  ij_freq <- table(as.character(rawdata[[origins.id]]), by = as.character(rawdata[[destinations.id]]))
  ij_freq_df <- as.data.frame(ij_freq)
  colnames(ij_freq_df) <- c("i_orig", "j_dest", "E_ij")

  ij_freq_df$E_ij <- ij_freq_df$E_ij+corObserved
  ij_freq_df$ij <- paste0(ij_freq_df$i_orig, "-", ij_freq_df$j_dest)
  ij_freq_df <- ij_freq_df[c(4,1,2,3)]

  ij_freq_sums <- aggregate (ij_freq_df$E_ij, by = list(ij_freq_df$i_orig), FUN = sum)
  colnames(ij_freq_sums) <- c("i_orig", "sum_E_ij")
  
  ij_freq_df <- merge(ij_freq_df, ij_freq_sums, by.x = "i_orig", by.y = "i_orig")
  ij_freq_df <- ij_freq_df[c(2,1,3,4,5)]

  
  ij_freq_df$p_ij <- ij_freq_df$E_ij/ij_freq_df$sum_E_ij
  

  addvars <- unlist(list(...)) 
  addvars_count <- length(addvars) 

  if (addvars_count > 0) {
    
    v <- 0
    
    for (v in 1:addvars_count) {
      addvar_abs_name <- paste0("E_ij_", addvars[v]) 
      addvar_total_name <- paste0("sum_E_ij_", addvars[v])
      addvar_p_ij <- paste0("p_ij_obs_", addvars[v]) 
  
      if (any(is.na(rawdata[[addvars[v]]])) == TRUE) {
        rawdata[is.na(rawdata[[addvars[v]]]),][[addvars[v]]] <- 0  
      }
      
      ij_v <- aggregate(as.numeric(rawdata[[addvars[v]]]), by = list(rawdata[[origins.id]], rawdata[[destinations.id]]), FUN = sum, na.rm = TRUE, drop = FALSE)

      colnames(ij_v) <- c("i_orig", "j_dest", addvar_abs_name)
      
      ij_v[is.na(ij_v[[addvar_abs_name]]),][[addvar_abs_name]] <- 0

      ij_v[[addvar_abs_name]] <- ij_v[[addvar_abs_name]]+corObserved
      
      ij_v_sums <- aggregate (ij_v[[addvar_abs_name]], by = list(ij_v$i_orig), FUN = sum, na.rm = TRUE)
      
      colnames(ij_v_sums) <- c("i_orig", addvar_total_name)
      
      ij_v <- merge(ij_v, ij_v_sums, by.x = "i_orig", by.y = "i_orig")

      ij_v$p_ij <- ij_v$E_ij/ij_v$sum_E_ij
      
      names(ij_v)[names(ij_v) == "p_ij"] <- addvar_p_ij
      
      ij_v$ij <- paste0(ij_v$i_orig, "-", ij_v$j_dest)
      
      ij_v <- ij_v[c(6,3,4,5)]
  
      ij_freq_df <- merge (ij_freq_df, ij_v, by.x = "ij", by.y = "ij")
    }
    
  }
  else {
    addvars_p_ij <- NULL
  }
  
  tcmat_fromto <- tcmat$tcmat[,3:4]
  
  mcimat <- merge (ij_freq_df, tcmat_fromto, by.x = "ij", by.y = "from_to", all.x = TRUE)
  names(mcimat)[names(mcimat) == "tc"] <- "d_ij"
  
  if (remNA == TRUE) {
    mcimat <- mcimat[complete.cases(mcimat),]
  }
  
  if ((!is.null(destvar.data)) & (!is.null(destvardata.id))) {
    mcimat <- merge (mcimat, destvar.data, by.x = "j_dest", by.y = destvardata.id, all.x = TRUE)
  }
  
  if ((!is.null(origvar.data)) & (!is.null(origvardata.id))) {
    mcimat <- merge (mcimat, origvar.data, by.x = "i_orig", by.y = origvardata.id, all.x = TRUE)
  }
  
  coords_origins <- tcmat$coords_origins
  coords_destinations <- tcmat$coords_destinations
  coords <- list (coords_origins = coords_origins, coords_destinations = coords_destinations)
  tc.mode <- tcmat$tc.mode
  mci.cormode <- list(remOrig = remOrig, remDest = remDest, corObserved = corObserved)

  invisible(list (mcimat = mcimat, mci.cormode = mci.cormode, coords = coords, tc.mode = tc.mode)) 

}
