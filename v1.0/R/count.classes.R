# Define what columns are Forest, non-forest, water... ####
count.classes <- function(proxy.table,
                          MAPBIOMAS = NULL,
                          PRODES = NULL,
                          num.forest = NULL,
                          num.non.forest = NULL,
                          num.urban = NULL,
                          num.mining = NULL,
                          num.pasture = NULL,
                          num.agriculture = NULL,
                          num.water = NULL,
                          num.others = NULL){

  # Dependencies ####
  # This function runs on base R only

  # MAPBIOMAS classes ####
  # MAPBIOMAS4
  if(MAPBIOMAS == 4){
    # general classes
    num.forest <- c(1, 2, 3)
    num.non.forest <- c(4, 5, 9, 10, 11, 12, 13, 14, 15, 18, 19, 20, 21, 22,
                        23, 24, 25, 29, 30, 32)
    num.water <- c(26, 31, 33)
    num.others <- c(0, 27)

    # non forrest detailed
    num.urban <- 24
    num.mining <- 30
    num.pasture <- 15
    num.agriculture <- 18
  }
  # MAPBIOMAS 7.1
  if(MAPBIOMAS == 7.1){
    # general classes
    num.forest <- c(1, 3)
    num.non.forest <- c(4, 5, 49, 10, 11, 12, 32, 29, 50, 13,
                        14, 15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 48, 9, 21,
                        22, 23, 24, 30, 25)
    num.water <- c(26, 31, 33)
    num.others <- c(0, 27)

    # non forrest detailed
    num.urban <- 24
    num.mining <- 30
    num.pasture <- 15
    num.agriculture <- c(9, 18, 19, 20, 36, 39, 40, 41, 46, 47, 48, 62)
  }
  # MAPBIOMAS 8.0
  if(MAPBIOMAS == 8){
    # general classes
    num.forest <- c(1, 3)
    num.non.forest <- c(4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13,
                        14, 15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21,
                        22, 23, 24, 30, 25)
    num.water <- c(26, 31, 33)
    num.others <- c(0, 27)

    # non forrest detailed
    num.urban <- 24
    num.mining <- 30
    num.pasture <- 15
    num.agriculture <- c(9, 18, 19, 20, 35, 36, 39, 40, 41, 46, 47, 48, 62)
  }


  # Function itself ####
  suppressWarnings({
    # Which columns are contained in each class (general)
    col.num.forest <- which(as.numeric(colnames(proxy.table)) %in% num.forest)
    col.num.non.forest <- which(as.numeric(colnames(proxy.table)) %in% num.non.forest)
    col.num.water <- which(as.numeric(colnames(proxy.table)) %in% num.water)
    col.num.others <- which(as.numeric(colnames(proxy.table)) %in% num.others)

    # Which columns are contained in each class (specific)
    col.num.urban <- which(as.numeric(colnames(proxy.table)) %in% num.urban)
    col.num.mining <- which(as.numeric(colnames(proxy.table)) %in% num.mining)
    col.num.pasture <- which(as.numeric(colnames(proxy.table)) %in% num.pasture)
    col.num.agriculture <- which(as.numeric(colnames(proxy.table)) %in% num.agriculture)
  })

  # Other classes columns (name, ID_mesh, year...)
  col.non.numeric <-
    c(1:ncol(proxy.table))[!(1:ncol(proxy.table) %in%
                               c(col.num.forest, col.num.non.forest,
                                 col.num.water, col.num.others,
                                 col.num.urban, col.num.mining,
                                 col.num.pasture, col.num.agriculture))]

  # Sum the values if the defined columns into a new column created
  proxy.table$Forest <-
    apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
      sum(as.numeric(x.apply[col.num.forest]), na.rm = TRUE)})

  proxy.table$NonForest <-
    apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
      sum(as.numeric(x.apply[col.num.non.forest]), na.rm = TRUE)})

  proxy.table$Water <-
    apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
      sum(as.numeric(x.apply[col.num.water]), na.rm = TRUE)})

  proxy.table$Others <-
    apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
      sum(as.numeric(x.apply[col.num.others]), na.rm = TRUE)})

  proxy.table$Agriculture <-
    apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
      sum(as.numeric(x.apply[col.num.agriculture]), na.rm = TRUE)})

  proxy.table$Mining <-
    apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
      sum(as.numeric(x.apply[col.num.mining]), na.rm = TRUE)})

  proxy.table$Pasture <-
    apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
      sum(as.numeric(x.apply[col.num.pasture]), na.rm = TRUE)})

  proxy.table$Urban <-
    apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
      sum(as.numeric(x.apply[col.num.urban]), na.rm = TRUE)})


  # Return ####
  # Return the new table with the classified columns
  return(proxy.table)
}
