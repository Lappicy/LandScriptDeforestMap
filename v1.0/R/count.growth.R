# Calculate growth of the classes created (Deforestation, Reforestation...) ####
count.growth <- function(proxy.table,
                         output.folder,
                         output.name,
                         column.used = NULL){

  # Dependencies ####
  require(sf)


  # Function itself ####
  # Calculate GROWTH for...

  # DEFORESTATION
  proxy.table$Deforestation[2:nrow(proxy.table)] <-
    base::diff(proxy.table[["Forest"]], lag = 1) * (-1)

  # REFORESTATION (when deforestation is negative)
  proxy.table$Reforestation <-
    ifelse(is.na(proxy.table[["Deforestation"]]) |
             proxy.table[["Deforestation"]] < 0,
           proxy.table[["Deforestation"]] * (-1),
           0)

  # GROWTH AGRICULTURE
  proxy.table$Growth_Agriculture[2:nrow(proxy.table)] <-
    base::diff(proxy.table[["Agriculture"]], lag = 1)

  # GROWTH MINING
  proxy.table$Growth_Mining[2:nrow(proxy.table)] <-
    base::diff(proxy.table[["Mining"]], lag = 1)

  # GROWTH PASTURE
  proxy.table$Growth_Pasture[2:nrow(proxy.table)] <-
    base::diff(proxy.table[["Pasture"]], lag = 1)

  # GROWTH URBAN
  proxy.table$Growth_Urban[2:nrow(proxy.table)] <-
    base::diff(proxy.table[["Urban"]], lag = 1)

  # First year as NA always!
  proxy.table[(proxy.table$Year == min(proxy.table$Year)),
              c("Deforestation", "Reforestation",
                "Growth_Urban", "Growth_Mining",
                "Growth_Pasture", "Growth_Agriculture")] <- NA

  # When growths are negative, make them equal zero
  proxy.table[which(proxy.table$Deforestation < 0), "Deforestation"] <- 0
  proxy.table[which(proxy.table$Growth_Agriculture < 0), "Growth_Agriculture"] <- 0
  proxy.table[which(proxy.table$Growth_Mining < 0), "Growth_Mining"] <- 0
  proxy.table[which(proxy.table$Growth_Pasture < 0), "Growth_Pasture"] <- 0
  proxy.table[which(proxy.table$Growth_Urban < 0), "Growth_Urban"] <- 0

  # Reorganize data
  # Columns that we want to use later
  suppressWarnings(col.num <- which(!is.na(as.numeric(colnames(proxy.table)))))
  first.col <- 1:(min(col.num) - 1)

  # Reorder columns
  proxy.table <- proxy.table[, c(colnames(proxy.table)[first.col],
                                 "Deforestation", "Reforestation",
                                 "Growth_Urban", "Growth_Mining",
                                 "Growth_Pasture", "Growth_Agriculture",
                                 "Forest", "NonForest", "Water", "Others",
                                 "Urban", "Mining", "Pasture", "Agriculture",
                                 sort(as.numeric(colnames(proxy.table)[col.num])))]


  # Save two files/tables as .txt (text) and two as .gpkg (vector) file ####
  # Save the final complete table
  write.table(x = st_drop_geometry(proxy.table),
              file = paste0(output.folder, output.name, ".txt"),
              quote = FALSE, sep = "\t", dec = ".", row.names = FALSE,
              fileEncoding = "UTF-8")

  # Save the final complete table as a .gpkg object (vector)
  sf::st_write(proxy.table, paste0(output.folder, output.name, ".gpkg"),
               encoding = "UTF-8", append = FALSE)

  # Save the table only with the classes
  write.table(x = st_drop_geometry(proxy.table[,c(1:which(colnames(proxy.table) ==
                                                            "Agriculture"))]),
              file = paste0(output.folder, "Simplified_", output.name, ".txt"),
              quote = FALSE, sep = "\t", dec = ".", row.names = FALSE,
              fileEncoding = "UTF-8")

  # Save the table only with the classes as a .gpkg object (vector)
  sf::st_write(proxy.table[,c(1:which(colnames(proxy.table) == "Agriculture"))],
               paste0(output.folder, "Simplified_", output.name, ".gpkg"),
               encoding = "UTF-8", append = FALSE)


  # Return ####
  # Return the complete table
  return(proxy.table)
}
