# Transform pixel to km2 ####
pixel.to.km2 <- function(proxy.table,
                         pixel.km2.ratio = 30*30/1000000){

  # Dependencies ####
  # This function uses base R only

  # Function itself ####
  # Which columns have pixel data (columns with numeric names)
  suppressWarnings(selected.col <- which(!is.na(as.numeric(colnames(proxy.table)))))

  # Multiply (either if it is a sf object or data.frame)
  suppressWarnings({
    proxy.table[,selected.col] <- proxy.table[selected.col] * pixel.km2.ratio
  })


  # Return ####
  # Return new table
  return(proxy.table)
}
