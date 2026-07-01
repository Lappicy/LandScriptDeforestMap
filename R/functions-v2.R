# Read geospatial file in R + reproject it ####
read.geo <- function(file.name, projection.wanted = 4326){

  # Libraries needed ####
  require(sf)


  # Function itself ####
  # If the data is already a sf object, change the column name to geometry and
  # return itself
  if(any(class(file.name) == "sf")){
    sf::st_geometry(file.name) <- "geometry"

    # Return it
    return(file.name)
  }

  # If it's not a sf object, read the file (with read_sf) and reproject it
  geo.file <-
    sf::read_sf(dsn = file.name) |>
    sf::st_transform(x = _, crs = projection.wanted)

  # Change the column name to geometry
  sf::st_geometry(geo.file) <- "geometry"

  # Change the geo.file into a data.frame and then as a sf
  # This guarantees the format
  geo.file <- sf::st_as_sf(as.data.frame(geo.file))


  # Return ####
  # Return this file as an sf object with geometry as a column name
  return(geo.file)
}


# Calculate the area of a SF object with the Albers projection ####
Albers.Area <- function(geo.file){

  # Libraries used ####
  require(sf)


  # Function itself ####
  # Albers projection
  # link South America Albers Equal Area Conic: https://epsg.io/102033
  albers.projection <-
    paste0("+proj=aea +lat_1=-5 +lat_2=-42 ",
           "+lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0",
           " +ellps=aust_SA +units=m no_defs")

  # Transform file (need to be sf) into the right projection
  geo.Albers <- sf::st_transform(geo.file, crs = albers.projection)

  # Calculate the area for all sf lines, in km^2
  # Put it as a new column of the ORIGINAL sf object
  geo.file$Area_km2 <- as.numeric(sf::st_area(geo.Albers))/1000000


  # Return ####
  # Return the original file with a new column named "Area_km2"
  return(geo.file)
}


# Close conections that might be using too much RAM temporary memory in R ####
close.geo.connections <- function(OS = "Windows"){

  # Function itself ####
  # If the OS is not windows, it might not run.. so don't run it
  if(!(toupper(OS) %in% c("WINDOWS", "WINDOW"))) stop("Only runs on Windows for now")

  # Close all the connections
  closeAllConnections()

  # Erase the temp files
  # Windows Environmental variable %USERPROFILE%\AppData\Local\Temp
  PCTempDir <- Sys.getenv("TEMP")

  # Detect and delete folders with pattern "Rtmp"
  folders <- dir(PCTempDir, pattern = "Rtmp", full.names = TRUE)
  unlink(folders, recursive = TRUE, force = TRUE, expand = TRUE)


  # Return (NULL) ####
  return(NULL)
}


# Make sure the projection of geospatial file when working with images ####
geo.tif.projection <- function(geo.file, tif.file){

  # Libraries used ####
  require(sf)


  # Function itself ####
  # Projection of both the geospatial and tif files
  tif.projection <- sf::st_crs(tif.file)
  geo.projection <- sf::st_crs(geo.file)

  # SE projeções forem diferentes, transformar o arquivo geoespacial
  # If the projections are different, reproject geospatial based on tif file
  if(geo.projection != tif.projection){

    # Mensagem avisando que o arquivo geoespacial foi reprojetado
    # Message warning that the geospatial file was reprojected
    message("Geospatial file reprojected from ",
            geo.projection$input, " to ",
            tif.projection$input)

    # Reproject the geo.file into the correct projection, saving on top
    geo.file <- sf::st_transform(x = geo.file, crs = tif.projection)
  }


  # Return ####
  # Returns geospatial file to the right projection
  return(geo.file)
}


# Create a mask for the images selected according to a classification given ####
raster.Classes <- function(tif.files,
                           classes,
                           class.name,
                           output.name){

  # Libraries used ####
  require(raster)


  # Function itself ####
  for(i.raster.Classes in 1:length(tif.files)){

    # Fechar conexoes
    closeAllConnections()

    # What is the file name
    year.proxy <- as.numeric(substr(tif.files[i.raster.Classes],
                                    nchar(tif.files[i.raster.Classes]) - 7,
                                    nchar(tif.files[i.raster.Classes]) - 4))

    # Pull the tif image as a raster file
    tif.raster <- raster::raster(tif.files[i.raster.Classes])

    # Change raster to binary form
    binary.raster <- tif.raster[[names(tif.raster)]] %in% classes

    # Export this binary raster
    raster::writeRaster(x = binary.raster,
                        filename = paste(output.name, year.proxy, class.name,
                                         ".tif", sep = "_"))

    # close unwanted connections
    close.geo.connections()
  }


  # Return (NULL) ####
  return(NULL)
}


# Create a mesh for a component (only squares supported for now) ####
create.mesh <- function(geo.file,
                        mesh.size = 0.25,
                        mesh.format = "square"){

  # Libraries used ####
  require(sf)


  # Function itself ####
  # Open geo.file
  geo.file <- read.geo(geo.file)

  # If mesh.size = NULL, return the normal shapefile
  if(is.null(mesh.size)){
    geo.file$ID_mesh <- 1:nrow(geo.file)
    sf::st_geometry(geo.file) <- "geom"
    return(geo.file)
    }

  # Squared grid
  if(tolower(mesh.format) == "square"){
      mesh.created <-
        sf::st_make_grid(x = geo.file,
                         cellsize = mesh.size) |>
        sf::st_as_sf(x = _)
  }

  # Create ID column
  mesh.created$ID_mesh <- 1:nrow(mesh.created)

  # Where it intersects with the geo.file
  mesh.map <- sf::st_intersection(x = mesh.created,
                                  y = geo.file)

  # ID names
  mesh.map$ID_mesh <- rownames(mesh.map) <- 1:nrow(mesh.map)

  # Change geometry column to "geom"
  sf::st_geometry(mesh.map) <- "geom"


  # Return ####
  # Returns the mesh.map where it intersects with the geo.file with ID column
  return(mesh.map)
}



# Based on a geospatial file, sum all classification of a raster ####
calc.raster <- function(geo.file, tif.file, year.used = NULL,
                        folder.output = "Proxy/", overwrite = TRUE){

  # Libraries used ####
  require(sf)
  require(raster)


  # Function itself ####
  # Open geo.file and tif
  geo.file <- read.geo(geo.file)
  tif.file <- raster::raster(tif.file)

  # Change the geo.file to only data.frame (lighter)
  geo.df <- sf::st_drop_geometry(geo.file)

  # Do the calculation for each row of this data.frame
  for(i.calc.raster in 1:nrow(geo.df)){

    # Check file names (if they already exist, don't run again)
    # Add 0s so all numbers have 10 characters
    name.calc.raster <- paste0("000000000", i.calc.raster)
    name.calc.raster <- substr(name.calc.raster,
                               nchar(name.calc.raster) - 9,
                               nchar(name.calc.raster))

    # If the file already exists, and OVERWRITE = TRUE, go to next "i"
    if(overwrite == FALSE){
      name.file.proxy <- paste0(folder.output, year.used, "_",
                                name.calc.raster, ".txt")

      if(file.exists(name.file.proxy)) next
    }

    # crop the tif.file and create a mask for it
    tif.crop <- raster::crop(x = tif.file, y = geo.file[i.calc.raster,])
    tif.mask <- raster::mask(x = tif.crop, mask = geo.file[i.calc.raster,])

    # Turn the raster into a matrix and get a table with summary data
    tif.matrix <- raster::as.matrix(tif.mask)
    tif.vector <- as.vector(tif.matrix)
    tif.classes <- base::table(tif.vector)

    # Create a data.frame from this
    tif.df <- data.frame(as.list(tif.classes), check.names = FALSE)

    # Create proxy table to jon geospatial with raster
    if(nrow(tif.df) > 0) proxy.table <- base::merge(geo.df[i.calc.raster,],
                                                    tif.df, all = TRUE)
    if(nrow(tif.df) == 0) proxy.table <- geo.df[i.calc.raster,]

    # Create year column
    proxy.table$Year <- year.used

    # Save file to folder.output (if it is not NULL)
    if(!is.null(folder.output)){

      # Add 0s so all numbers have 10 characters
      name.calc.raster <- paste0("000000000", i.calc.raster)
      name.calc.raster <- substr(name.calc.raster,
                                 nchar(name.calc.raster) - 9,
                                 nchar(name.calc.raster))

      # If the output doesnt exist, create it
      dir.create(file.path(getwd(), folder.output), showWarnings = FALSE)

      # Save the file with the name
      write.table(x = proxy.table,
                  file = paste0(folder.output, year.used, "_",
                                name.calc.raster, ".txt"),
                  sep = "\t", dec = ".", row.names = FALSE, quote = FALSE,
                  fileEncoding = "UTF-8")
    }

    # Create final table (merge proxys)
    if(i.calc.raster == 1) final.table <- proxy.table
    if(i.calc.raster != 1) final.table <- base::merge(final.table, proxy.table,
                                                      all = TRUE)
  }

  # Reorder columns in an alfa numeric manner
  # order(as.numeric(colnames(final.table)))
  alfa.named.columns <- which(is.na(as.numeric(colnames(final.table))))
  numeric.named.columns <- which(!is.na(as.numeric(colnames(final.table))))
  numeric.named.columns <- sort(as.numeric(colnames(final.table)[numeric.named.columns]))
  numeric.named.columns <- match(as.character(numeric.named.columns), colnames(final.table))
  final.table <- final.table[,c(alfa.named.columns, numeric.named.columns)]


  # Return ####
  # Return the final table for all the lines from a geospatial file
  return(final.table)
}


# Join all files created on calc.raster into one big file ####
join.files <- function(folder.calc.raster,
                       new.folder,
                       output.name,
                       column.order = NULL){

  # Libraries required ####
  require(dplyr)


  # Function itself ####
  # Pull and organize files
  # Which files in the folder has the format defined by the function
  # Quais arquivos existem no diretorio do formato definido pela função
  files.names <- list.files(folder.calc.raster,
                            pattern = paste0("\\", ".txt", "$"),
                            full.names = T)

  # Use lapply to get all these files
  # Fazer um lapply pra pegar todos esses arquivos
  files.list <- lapply(files.names, read.table,
                       header = TRUE, sep = "\t", dec = ".",
                       fileEncoding = "UTF-8", check.names = FALSE)

  # Change the list to a single data.frame with all the columns
  files.df <- dplyr::bind_rows(files.list)

  # Order the columns
  files.df <- files.df[,order(as.numeric(colnames(files.df)))]

  # Take away the rownames
  rownames(files.df) <- NULL

  # Save file
  write.table(x = files.df,
              file = paste0(new.folder, output.name),
              quote = FALSE, sep = "\t", dec = ".", row.names = FALSE,
              fileEncoding = "UTF-8")


  # Return ####
  # Return the data.frame
  return(arq_final)
}



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
                          num.fire = NULL,
                          num.others = NULL){

  # MAPBIOMAS classes ####
  if(!is.null(MAPBIOMAS)){
    # MAPBIOMAS4
    if(MAPBIOMAS == 4 ){
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
    
    # MAPBIOMAS 8
    if(MAPBIOMAS == 8){
      # general classes
      num.forest <- c(1, 3)
      num.non.forest <- c(4, 5, 6, 49,
                          10, 11, 12, 32, 29, 50, 13,
                          14, 15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21,
                          22, 23, 24, 30, 25)
      num.water <- c(26, 31, 33)
      num.others <- c(0, 27)

      # non forrest detailed
      num.urban <- 24
      num.mining <- 30
      num.pasture <- 15
      num.agriculture <- c(9, 18, 19, 20, 36, 39, 40, 41, 46, 47, 48, 62)
    }
    
    # MAPBIOMAS 10
    if(MAPBIOMAS == 10){
      # classes gerais
      num.forest <- c(1, 3)
      num.non.forest <- c(4, 5, 6, 49, 10, 11, 12, 32, 29, 50,
                          14, 15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21,
                          22, 23, 24, 30, 75, 25)
      num.water <- c(26, 31, 33)
      num.others <- c(0, 27)

      # detalhes do não floresta
      num.urban <- 24
      num.mining <- 30
      num.crop.livestock <- 14
      num.pasture <- 15
      num.agriculture <- c(9, 18, 19, 20, 21, 35, 36, 39, 40, 41, 46, 47, 48, 62)
    }
  }


  # Function itself ####
  # Which columns are contained in each class (general)
  col.num.forest <-
    which(as.numeric(colnames(proxy.table)) %in% num.forest)
  col.num.non.forest <-
    which(as.numeric(colnames(proxy.table)) %in% num.non.forest)
  col.num.water <-
    which(as.numeric(colnames(proxy.table)) %in% num.water)
  col.num.others <-
    which(as.numeric(colnames(proxy.table)) %in% num.others)

  # Which columns are contained in each class (specific)
  col.num.urban <-
    which(as.numeric(colnames(proxy.table)) %in% num.urban)
  col.num.mining <-
    which(as.numeric(colnames(proxy.table)) %in% num.mining)
  col.num.pasture <-
    which(as.numeric(colnames(proxy.table)) %in% num.pasture)
  col.num.agriculture <-
    which(as.numeric(colnames(proxy.table)) %in% num.agriculture)
  col.num.crop.livestock <-
    which(as.numeric(colnames(proxy.table)) %in% num.crop.livestock)
  col.num.fire <-
    which(as.numeric(colnames(proxy.table)) %in% num.fire)


  # Sum the values if the defined columns into a new column created
  if(length(col.num.forest) != 0){
    proxy.table$Forest <-
      apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
        sum(as.numeric(x.apply[col.num.forest]), na.rm = TRUE)})
  }

  if(length(col.num.non.forest) != 0){
    proxy.table$NonForest <-
      apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
        sum(as.numeric(x.apply[col.num.non.forest]), na.rm = TRUE)})
  }

  if(length(col.num.water) != 0){
    proxy.table$Water <-
      apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
        sum(as.numeric(x.apply[col.num.water]), na.rm = TRUE)})
  }

  if(length(col.num.others) != 0){
    proxy.table$Others <-
      apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
        sum(as.numeric(x.apply[col.num.others]), na.rm = TRUE)})
  }

  if(length(col.num.agriculture) != 0){
    proxy.table$Agriculture <-
      apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
        sum(as.numeric(x.apply[col.num.agriculture]), na.rm = TRUE)})
  }

  if(length(col.num.mining) != 0){
    proxy.table$Mining <-
      apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
        sum(as.numeric(x.apply[col.num.mining]), na.rm = TRUE)})
  }

  if(length(col.num.pasture) != 0){
    proxy.table$Pasture <-
      apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
        sum(as.numeric(x.apply[col.num.pasture]), na.rm = TRUE)})
  }

  if(length(col.num.urban) != 0){
    proxy.table$Urban <-
      apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
      sum(as.numeric(x.apply[col.num.urban]), na.rm = TRUE)})
  }
  
  if(length(col.num.crop.livestock) != 0){
    proxy.table$CropLivestock <-
      apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
        sum(as.numeric(x.apply[col.num.crop.livestock]), na.rm = TRUE)})
  }

  if(length(col.num.fire) != 0){
    proxy.table$Fire <-
      apply(X = proxy.table, MARGIN = 1, FUN = function(x.apply){
        sum(as.numeric(x.apply[col.num.fire]), na.rm = TRUE)})
  }

  # Return ####
  # Return the new table with the classified columns
  return(proxy.table)
}


# Calculate growth of the classes created (Deforestation, Reforestation...) ####
count.growth <- function(proxy.table,
                         output.folder,
                         output.name,
                         column.used = NULL){

  # Function ifself ####
  # Organize data ####
  # Number of diferent columns used
  n.types.table <- unique(proxy.table[,column.used])

  # Columns that we want to use later
  suppressWarnings({col.num <- which(!is.na(as.numeric(colnames(proxy.table))))})
  first.col <- 1:(min(col.num) - 1)
  
  # Columns with names
  suppressWarnings({
    col.names.num <- which(is.na(as.numeric(colnames(proxy.table))))
    col.names.num <- col.names.num[!(col.names.num %in% first.col)]
  })
  
  # New columns number
  new.columns.num <- length(col.num) + length(col.names.num)

  # Create empty columns for each class
  proxy.table[,(ncol(proxy.table)+1):(ncol(proxy.table) + new.columns.num)] <- NA
  names(proxy.table)[utils::tail(seq_along(names(proxy.table)), new.columns.num)] <-
    paste0("Variation_", colnames(proxy.table)[c(col.num, col.names.num)])


  # Calculate growth ####
  variation.cols <- base::grep("^Variation_", names(proxy.table))

  # Loop for each "Variation_" column
  proxy.table2 <- proxy.table
  proxy.table2[is.na(proxy.table2)] <- 0

  for(i in c(col.num, col.names.num)){
    # Which class we are working with
    class.id <- colnames(proxy.table)[i]

    # Which Variation_ column represents this class
    variation.class.id <-
      match(class.id, sub("^Variation_", "", colnames(proxy.table)[variation.cols]))
    variation.col.id <- variation.cols[variation.class.id]

    # Calculate the Variation (Variation_)
    proxy.table[2:nrow(proxy.table), variation.col.id] <-
      base::diff(proxy.table2[, i], lag = 1)
  }; remove(proxy.table2)

  # First year is always NA
  proxy.table[(proxy.table$Year == min(proxy.table$Year)), c(variation.cols)] <- NA

  # If there is a column for "Forest", calculate Deforestation
  # Deforestation = -Variation_Forest & > 0
  if(any(colnames(proxy.table) == "Variation_Forest")){
    proxy.table$Deforestation <- (-1) * proxy.table$Variation_Forest
    proxy.table$Deforestation[proxy.table$Deforestation < 0] <- 0
  }

  
  # Save two files/tables as a .txt file####
  # Save the final complete table
  write.table(x = proxy.table,
              file = paste0(output.folder, output.name),
              quote = FALSE, sep = "\t", dec = ".", row.names = FALSE,
              fileEncoding = "UTF-8")

  # Save the table only with the classes
  write.table(x = proxy.table[,is.na(as.numeric(colnames(proxy.table)))],
              file = paste0(output.folder, "Simplified_", output.name),
              quote = FALSE, sep = "\t", dec = ".", row.names = FALSE,
              fileEncoding = "UTF-8")


  # Return ####
  # Return the complete table
  return(proxy.table)
}


# End -------------------------------------------------------------------------
# Complete function (Growth.Analysis) ####
Growth.Analysis <-
  function(geo.file, tif.folder,
           mesh.size,
           output.folder, output.name,
           MAPBIOMAS = NULL, PRODES = NULL, ...){

    # Function itself ####
    # Read geo.file
    geo.file <- read.geo(file.name = geo.file)

    # Read one tif.file
    tif.file.proj <- raster::raster(list.files(tif.folder, full.names = TRUE)[1])

    # Reproject the geo.file if needed
    if(st_crs(geo.file) != st_crs(tif.file.proj)){
      geo.file <- geo.tif.projection(geo.file = geo.file, tif.file = tif.file.proj)
    }

    # Create mesh
    mesh.geo.file <- create.mesh(geo.file = geo.file, mesh.size = mesh.size)

    # Get the complete files names where the tif.files are located (tif.folder)
    tif.files.names <- list.files(tif.folder, full.names = TRUE)

    # Calculate the classes for each raster file
    for(i in 1:length(tif.files.names)){

      year.proxy <- strsplit(tif.files.names[i], "_")[[1]][3]
      #year.proxy <- substr(tif.files.names[i],
      #                     nchar(tif.files.names[i]) - 7,
      #                     nchar(tif.files.names[i]) - 4)

      cat("Rodando para o ano: ", year.proxy, " ... ", format(Sys.time(), "%d/%m/%Y %X"), "\n")

      raster.data.proxy <- calc.raster(geo.file = mesh.geo.file,
                                       tif.file = tif.files.names[i],
                                       year.used = year.proxy)

      if(i == 1) raster.data <- raster.data.proxy
      if(i > 1) raster.data <- base::merge(raster.data, raster.data.proxy,
                                           all = TRUE)

      remove(year.proxy, raster.data.proxy)
    }

    # If the directory doesn't exist, create it
    dir.create(file.path(getwd(), output.folder), showWarnings = FALSE)

    # Save this file (because it is time consuming) and remove folder
    write.table(x = raster.data,
                file = paste0(output.folder, "CalcRasterPixels.txt"),
                dec = ".", sep = "\t", quote = F, col.names = T, row.names = F,
                fileEncoding = "UTF-8")
    unlink("Proxy/", recursive = TRUE)


    # Transform pixel to km2
    raster.data.km2 <- pixel.to.km2(proxy.table = raster.data)

    # Count classes
    raster.data.km2.classes <- count.classes(proxy.table = raster.data.km2,
                                             MAPBIOMAS = MAPBIOMAS,
                                             PRODES = PRODES)

    # Calculate growth
    raster.data.km2.growth <- count.growth(proxy.table = raster.data.km2.classes,
                                           output.folder = output.folder,
                                           output.name = output.name)

    # Make the df to geopackage
    final.table <- base::merge(mesh.geo.file, raster.data.km2.growth)

    # Save as geopackage
    sf::st_write(final.table, append = FALSE,
                 paste0(output.folder,
                        substr(output.name, 1, nchar(output.name) - 4),
                        ".gpkg"))

    # Save as geopackage the simplified version
    sf::st_write(final.table[,!is.na(as.numeric(colnames(final.table)))],
                 append = FALSE,
                 paste0(output.folder, "simplified_",
                        substr(output.name, 1, nchar(output.name) - 4),
                        ".gpkg"))

    # Return ####
    return(final.table)
  }
