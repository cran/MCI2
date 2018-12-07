to.tcmat <-
function (dataset, colname.from, colname.to, colname.tc) {
  
  checkod (colname.from, colname.to)
  
  fromto <- paste0(dataset[[colname.from]], "-", dataset[[colname.to]])
  
  tcmat <- data.frame(dataset[[colname.from]], dataset[[colname.to]], fromto, dataset[[colname.tc]])
  
  colnames(tcmat) <- c("from", "to", "from_to", "tc")
  
  coords_origins <- "External transport costs matrix dataset"
  coords_destinations <- "External transport costs matrix dataset"
  tc.mode <- "External transport costs matrix dataset"
  
  invisible(list (tcmat = tcmat, coords_origins = coords_origins, coords_destinations = coords_destinations, tc.mode = tc.mode))
  
}