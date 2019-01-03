rawdata.prep <- 
function (cols.below1, cols.below2, cols.keep, colnames.new) {

  num_cols <- length(cols.below1)
  num_colskeep <- length(cols.keep)
  
  rawdata_nrow <- nrow(as.data.frame(cols.below1[1]))
  
  newdata <- data.frame(matrix(ncol = (num_colskeep+1), nrow = (rawdata_nrow*num_cols)))
  
  newdata_col <- 1
  i <- 0
  
  for (i in 1:num_cols) {
    newdata[newdata_col:rawdata_nrow,1] <- cols.below1[i]
    newdata[newdata_col:rawdata_nrow,2:((2+num_colskeep)-1)] <- cols.keep
    
    newdata_col <- rawdata_nrow+1
    rawdata_nrow <- rawdata_nrow+rawdata_nrow
  }
  
  newdata1 <- newdata[,1]
  
  num_cols <- length(cols.below2)
  
  rawdata_nrow <- nrow(as.data.frame(cols.below2[1]))
  newdata <- data.frame(matrix(ncol = (num_colskeep+1), nrow = (rawdata_nrow*num_cols)))
  
  newdata_col <- 1
  i <- 0
  
  for (i in 1:num_cols) {
    newdata[newdata_col:rawdata_nrow,1] <- cols.below2[i]
    newdata[newdata_col:rawdata_nrow,2:((2+num_colskeep)-1)] <- cols.keep
    
    newdata_col <- rawdata_nrow+1
    rawdata_nrow <- rawdata_nrow+rawdata_nrow
  }
  
  newdata2 <- newdata
  
  newdata_all <- cbind(newdata1, newdata2)
  
  colnames(newdata_all) <- colnames.new 
  
  return(newdata_all)
  
}

