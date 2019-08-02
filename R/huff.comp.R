huff.comp <- function(huffmodel1, huffmodel2) {
  
  hufftotal1 <- huffmodel1$hufftotal
  hufftotal2 <- huffmodel2$hufftotal
  
  hufftotal_merge <- merge (hufftotal1, hufftotal2, by.x = "j_dest", by.y = "j_dest", all.x = TRUE, all.y = TRUE)
  
  hufftotal_merge[is.na(hufftotal_merge)] <- 0
  
  hufftotal_merge[,6] <- hufftotal_merge[,4]-hufftotal_merge[,2]
  hufftotal_merge[,7] <- hufftotal_merge[,5]-hufftotal_merge[,3]
  hufftotal_merge[,8] <- hufftotal_merge[,6]/hufftotal_merge[,2]*100

  colnames(hufftotal_merge) <- c("j_dest", "T_j1", "T_j_share1", "T_j2", "T_j_share2", "T_j_change", "T_j_share_change", "T_j_change_perc")
  
  
  cat("\n")
  cat("Huff Model", "\n")
  cat("Comparing of results", "\n")
  cat("\n")
  cat("Total results", "\n")
  print(as.data.frame(hufftotal_merge))
  cat("\n")
  
  
  invisible(hufftotal_merge)
}
