model.export <-
function (huffmodel, mat.filename, total.filename, decimal = ".", colsep = ";", mat.ascrosstab = TRUE) {
  

  modelmat <- huffmodel$huffmat
  
  if (mat.ascrosstab == FALSE) {
    write.table (modelmat, paste0(mat.filename, ".csv"), dec = decimal, sep = colsep, row.names = FALSE, fileEncoding = "UTF-8")
  }
  
  else {
  
  modelmat_crosstab <- ijmatrix.crosstab(modelmat, "i_orig", "j_dest", "p_ij")
  
  modelmat_crosstab <- merge(modelmat_crosstab, huffmodel$coords$coords_origins, by.x = "submarkets", by.y = "origins.id")
  
  names(modelmat_crosstab)[names(modelmat_crosstab) == "submarkets"] <- "origins.id"
  
  write.table (modelmat_crosstab, paste0(mat.filename, ".csv"), dec = decimal, sep = colsep, row.names = FALSE, fileEncoding = "UTF-8")
  }
  

  totals <- huffmodel$hufftotal
  
  totals <- merge (totals, huffmodel$coords$coords_destinations, by.x = "j_dest", by.y = "destinations.id")
  
  write.table (totals, paste0(total.filename, ".csv"), dec = decimal, sep = colsep, row.names = FALSE, fileEncoding = "UTF-8")
 
}
