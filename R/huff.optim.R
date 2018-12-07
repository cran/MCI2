huff.optim <- 
function (huffmodel, location.dataset, location.id, location.total,
          tolerance = 5, iterations = 3, show_proc = TRUE) {
  
  huffdataset <- huffmodel$huffmat
  
  lambda <- huffmodel$params[2,1]
  lambda2 <- huffmodel$params[2,2]
  dtype <- rownames(huffmodel$params)[1]

  localmarket <- aggregate(huffmodel$huffmat$C_i, by = list(huffmodel$huffmat$i_orig), FUN = mean)
  colnames(localmarket) <- c("origins.id", "origins.pot")


  i <- 0
  
  locations_count <- nrow(location.dataset)
  
  model_diag_df <- data.frame()
  
  huffworkfile <- huffdataset
  
  for (i in 1:iterations)
  {
    
    if (show_proc == TRUE) { 
      cat ("Iteration", i, "of", iterations, "...", "\n")
    }
    
    huffworkfile_total <- huff.attrac(huffdataset = huffworkfile, origins = "i_orig", locations = "j_dest", 
                                      attrac = "A_j", dist = "d_ij",
                                      lambda = lambda, dtype = dtype, lambda2 = lambda2, 
                                      localmarket_dataset = localmarket, origin_id = "origins.id", localmarket = "origins.pot", 
                                      location.dataset, location.id, location.total, 
                                      tolerance = tolerance, output = "total", show_proc = show_proc,
                                      check_df = FALSE)
    
    locations_single <- huffworkfile_total$suppliers_single

    model_diag <- model.fit(huffworkfile_total$total_obs, huffworkfile_total$sum_E_j)
    model_diag_df <- rbind(model_diag_df, as.data.frame(model_diag))

    huffworkfile_total_attrac_new <- data.frame (huffworkfile_total$suppliers_single, huffworkfile_total$attrac_new_opt)
    colnames(huffworkfile_total_attrac_new) <- c("suppliers_single", "attrac_new_opt")

    huffworkfile <- merge (huffworkfile, huffworkfile_total_attrac_new, by.x = "j_dest", by.y = "suppliers_single")

    huffworkfile$A_j <- huffworkfile$attrac_new_opt

    huffworkfile$total_obs <- NULL
    huffworkfile$diff <- NULL
    huffworkfile$attrac_new_opt <- NULL

  }
  

  huffworkfile_shares_final <- huff.shares(huffworkfile, "i_orig", "j_dest", "A_j", "d_ij", lambda = lambda, dtype = dtype, lambda2 = lambda2, check_df = FALSE)

  iterations_count <- 1:iterations
  huffworkfile_total_diag <- data.frame(iterations_count, model_diag_df)

  huffmat <- huffworkfile_shares_final[c(2, 3, 4, 1, 5, 6, 7, 8, 9)]
  hufftotal <- huffworkfile_total
  colnames(hufftotal) <- c("j_dest", "T_j", "T_j_share", "T_j_obs", "T_j_diff", "A_opt")
  
  return(list (huffmat = huffmat, hufftotal = hufftotal, diag = huffworkfile_total_diag,
               params = huffmodel$params, coords = huffmodel$coords, tc.mode = huffmodel$tc.mode))
  
}