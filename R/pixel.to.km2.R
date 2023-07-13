# Transform pixel to km2 ####
pixel.to.km2 <- function(proxy.table,
                         pixel.km2.ratio = 30*30/1000000){

  # Which columns have pixel data (columns with numeric names)
  selected.col <- which(!is.na(as.numeric(colnames(proxy.table))))

  # Multiply
  proxy.table[,selected.col] <- proxy.table[,selected.col] * pixel.km2.ratio

  # Return new table
  return(proxy.table)
}