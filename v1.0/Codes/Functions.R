# Read vector (geospatial) file in R + reproject it ####
read.geo <- function(file.name, projection.wanted = 4326){

  # Dependencies ####
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


# Read raster (image) file in R and stack it into a list ####
read.raster <- function(raster.file){

  # Dependencies ####
  require(raster)


  # Function itself ####
  # If the data is a RasterStack, unstack it
  if(class(raster.file) == "RasterStack") return(raster::unstack(raster.file))

  # If the data is a list, check if it is a named list or a raster list
  if(class(raster.file) == "list"){

    # If already a list of raster, return it
    if(any(class(raster.file[[1]]) == "RasterLayer")) return(raster.file)

    # If the list is a character list
    if(any(class(raster.file[[1]]) == "character")){

      # Read files and turn into a RasterStack
      return(lapply(raster.file, FUN = function(x.apply){raster::raster(x.apply)}))

    }
  }


  # If the data is a folder name (character) read all the info from that folder
  if(any(class(raster.file) == "character")){

    # List the files, read them and create a list of Rasters
    raster.list <-
      list.files(raster.file, full.names = TRUE) |>
      lapply(FUN = function(x.apply){raster::raster(x.apply)})

    # Return it
    return(raster.list)
  }

  # If not a list, RasterLayer, RasterStack or character vector, stops function
  stop("Raster class unidentified!")
}


# Calculate the area of a SF object with the Albers projection ####
Albers.Area <- function(geo.file){

  # Dependencies ####
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

  # Dependencies ####
  # This function runs on base R only


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


# Make sure the projection of vector (geospatial) file when working with images ####
geo.tif.projection <- function(geo.file, tif.file){

  # Dependencies ####
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

  # Dependencies ####
  require(raster)


  # Function itself ####
  for(i.raster.Classes in 1:length(tif.files)){

    # Fechar conexoes
    closeAllConnections()

    # What is the file name
    year.proxy <- base::as.numeric(substr(tif.files[i.raster.Classes],
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

  # Dependencies ####
  require(sf)


  # Function itself ####
  # Open geo.file
  geo.file <- read.geo(geo.file)

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
  sf::st_geometry(mesh.map) <- "geometry"


  # Return ####
  # Returns the mesh.map where it intersects with the geo.file with ID column
  return(mesh.map)
}



# Based on a vector (geospatial) file, sum all classification of a raster ####
calc.raster <- function(geo.file,
                        tif.file,
                        year.used = NULL,
                        folder.output = "Proxy/"){

  # Dependencies ####
  require(sf)
  require(raster)


  # Function itself ####
  # Open geo.file
  geo.file <- read.geo(geo.file)

  # Reproject geo.file if needed
  geo.file <- geo.tif.projection(geo.file = geo.file, tif.file = tif.file)

  # Do the calculation for each row of this data.frame
  proxy.table <-
    lapply(st_geometry(geo.file), FUN = function(x.calc.raster){

      # Formato apropriado
      x.calc.raster.sf <- st_as_sf(st_sfc(x.calc.raster, crs = st_crs(geo.file)))

      # crop the tif.file and create a mask for it
      tif.mask <- raster::mask(x = raster::crop(x = tif.file,
                                                y = x.calc.raster.sf),
                               mask = x.calc.raster.sf)

      # Turn the raster into a matrix and get a table with summary data
      tif.classes <- as.data.frame(base::table(raster::as.matrix(tif.mask)))

      # Make it into a data.frame with a column for ID_mesh
      if(nrow(tif.classes) == 0){
        tif.df <-
          data.frame(ID_mesh = geo.file$ID_mesh[geo.file$geom %in% x.calc.raster.sf$x])
      }

      if(nrow(tif.classes) != 0){
        tif.df <- data.frame(t(tif.classes[,2]))
        colnames(tif.df) <- as.character(tif.classes[,1])
        tif.df$ID_mesh <- geo.file$ID_mesh[geo.file$geom %in% x.calc.raster.sf$x]
      }

      # Merge the data.frame with the geo.file (only the specific ID_mesh)
      proxy.table <- base::merge(geo.file, tif.df, all.x = F, all.y = T)
      proxy.table$Year <- year.used
      proxy.table <- proxy.table[,c(ncol(proxy.table), 1:(ncol(proxy.table) - 1))]

      # Save file to folder.output (if it is not NULL)
      if(!is.null(folder.output)){

        # if the number is less than 10, add three zeros
        # if the number is less than 100, add two zeros
        # if the number is less than 1000, add one zero
        name.calc.raster <-
          ifelse(proxy.table$ID_mesh < 10, paste0("000", proxy.table$ID_mesh ),
                 ifelse(proxy.table$ID_mesh  < 100, paste0("00", proxy.table$ID_mesh ),
                        ifelse(proxy.table$ID_mesh  < 1000, paste0("0", proxy.table$ID_mesh ),
                               proxy.table$ID_mesh )))

        # If the output doesnt exist, create it
        dir.create(file.path(getwd(), folder.output), showWarnings = FALSE)

        # Save the file with the name
        write.table(x = proxy.table,
                    file = paste0(folder.output, year.used, "_",
                                  name.calc.raster, ".txt"),
                    sep = "\t", dec = ".", row.names = FALSE, quote = FALSE,
                    fileEncoding = "UTF-8")
      }

      # Returns the table for that ID_mesh
      return(proxy.table)
    })

  # Change the list to a single data.frame with all the columns
  final.table <- dplyr::bind_rows(proxy.table)

  # Reorder columns in an alfa numeric manner
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

  # Dependencies ####
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


# End -------------------------------------------------------------------------
# Complete function (Growth.Analysis) ####
Growth.Analysis <-
  function(geo.file, tif.folder,
           mesh.size,
           output.folder, output.name,
           MAPBIOMAS = NULL, PRODES = NULL, ...){

    # Functions used ####
    {
      # Read vector (geospatial) file in R + reproject it ####
      read.geo <- function(file.name, projection.wanted = 4326){

        # Dependencies ####
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


      # Read raster (image) file in R and stack it into a list ####
      read.raster <- function(raster.file){

        # Dependencies ####
        require(raster)


        # Function itself ####
        # If the data is a RasterStack, unstack it
        if(class(raster.file) == "RasterStack") return(raster::unstack(raster.file))

        # If the data is a list, check if it is a named list or a raster list
        if(class(raster.file) == "list"){

          # If already a list of raster, return it
          if(any(class(raster.file[[1]]) == "RasterLayer")) return(raster.file)

          # If the list is a character list
          if(any(class(raster.file[[1]]) == "character")){

            # Read files and turn into a RasterStack
            return(lapply(raster.file, FUN = function(x.apply){raster::raster(x.apply)}))

          }
        }


        # If the data is a folder name (character) read all the info from that folder
        if(any(class(raster.file) == "character")){

          # List the files, read them and create a list of Rasters
          raster.list <-
            list.files(raster.file, full.names = TRUE) |>
            lapply(FUN = function(x.apply){raster::raster(x.apply)})

          # Return it
          return(raster.list)
        }

        # If not a list, RasterLayer, RasterStack or character vector, stops function
        stop("Raster class unidentified!")
      }


      # Close conections that might be using too much RAM temporary memory in R ####
      close.geo.connections <- function(OS = "Windows"){

        # Dependencies ####
        # This function runs on base R only


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


      # Create a mesh for a component (only squares supported for now) ####
      create.mesh <- function(geo.file,
                              mesh.size = 0.25,
                              mesh.format = "square"){

        # Dependencies ####
        require(sf)


        # Function itself ####
        # Open geo.file
        geo.file <- read.geo(geo.file)

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
        sf::st_geometry(mesh.map) <- "geometry"


        # Return ####
        # Returns the mesh.map where it intersects with the geo.file with ID column
        return(mesh.map)
      }


      # Make sure the projection of vector (geospatial) file when working with images ####
      geo.tif.projection <- function(geo.file, tif.file){

        # Dependencies ####
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


      # Based on a vector (geospatial) file, sum all classification of a raster ####
      calc.raster <- function(geo.file,
                              tif.file,
                              year.used = NULL,
                              folder.output = "Proxy/"){

        # Dependencies ####
        require(sf)
        require(raster)


        # Function itself ####
        # Open geo.file
        geo.file <- read.geo(geo.file)

        # Reproject geo.file if needed
        geo.file <- geo.tif.projection(geo.file = geo.file, tif.file = tif.file)

        # Do the calculation for each row of this data.frame
        proxy.table <-
          lapply(st_geometry(geo.file), FUN = function(x.calc.raster){

            # Formato apropriado
            x.calc.raster.sf <- st_as_sf(st_sfc(x.calc.raster, crs = st_crs(geo.file)))

            # crop the tif.file and create a mask for it
            tif.mask <- raster::mask(x = raster::crop(x = tif.file,
                                                      y = x.calc.raster.sf),
                                     mask = x.calc.raster.sf)

            # Turn the raster into a matrix and get a table with summary data
            tif.classes <- as.data.frame(base::table(raster::as.matrix(tif.mask)))

            # Make it into a data.frame with a column for ID_mesh
            if(nrow(tif.classes) == 0){
              tif.df <-
                data.frame(ID_mesh = geo.file$ID_mesh[geo.file$geom %in% x.calc.raster.sf$x])
            }

            if(nrow(tif.classes) != 0){
              tif.df <- data.frame(t(tif.classes[,2]))
              colnames(tif.df) <- as.character(tif.classes[,1])
              tif.df$ID_mesh <- geo.file$ID_mesh[geo.file$geom %in% x.calc.raster.sf$x]
            }

            # Merge the data.frame with the geo.file (only the specific ID_mesh)
            proxy.table <- base::merge(geo.file, tif.df, all.x = F, all.y = T)
            proxy.table$Year <- year.used
            proxy.table <- proxy.table[,c(ncol(proxy.table), 1:(ncol(proxy.table) - 1))]

            # Save file to folder.output (if it is not NULL)
            if(!is.null(folder.output)){

              # if the number is less than 10, add three zeros
              # if the number is less than 100, add two zeros
              # if the number is less than 1000, add one zero
              name.calc.raster <-
                ifelse(proxy.table$ID_mesh < 10, paste0("000", proxy.table$ID_mesh ),
                       ifelse(proxy.table$ID_mesh  < 100, paste0("00", proxy.table$ID_mesh ),
                              ifelse(proxy.table$ID_mesh  < 1000, paste0("0", proxy.table$ID_mesh ),
                                     proxy.table$ID_mesh )))

              # If the output doesnt exist, create it
              dir.create(file.path(getwd(), folder.output), showWarnings = FALSE)

              # Save the file with the name
              write.table(x = proxy.table,
                          file = paste0(folder.output, year.used, "_",
                                        name.calc.raster, ".txt"),
                          sep = "\t", dec = ".", row.names = FALSE, quote = FALSE,
                          fileEncoding = "UTF-8")
            }

            # Returns the table for that ID_mesh
            return(proxy.table)
          })

        # Change the list to a single data.frame with all the columns
        final.table <- dplyr::bind_rows(proxy.table)

        # Reorder columns in an alfa numeric manner
        alfa.named.columns <- which(is.na(as.numeric(colnames(final.table))))
        numeric.named.columns <- which(!is.na(as.numeric(colnames(final.table))))
        numeric.named.columns <- sort(as.numeric(colnames(final.table)[numeric.named.columns]))
        numeric.named.columns <- match(as.character(numeric.named.columns), colnames(final.table))
        final.table <- final.table[,c(alfa.named.columns, numeric.named.columns)]


        # Return ####
        # Return the final table for all the lines from a geospatial file
        return(final.table)
      }


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


    }


    # Dependencies ####
    require(dplyr)
    require(raster)
    require(sf)


    # Function itself ####
    # Read geo.file
    geo.file <- read.geo(file.name = geo.file)

    # Create mesh
    mesh.geo.file <- create.mesh(geo.file = geo.file, mesh.size = mesh.size)

    # Read the tif.file into RasterStack
    tif.list <- read.raster(raster.file = tif.folder)

    # Calculate the classes for each raster file
    raster.data <-
      lapply(X = tif.list, FUN = function(x.int){

        # Pegar os últimos 4 caracteres do nome
        year.proxy <- substr(x = x.int[[1]]@data@names,
                             start = nchar(x.int[[1]]@data@names) - 3,
                             stop = nchar(x.int[[1]]@data@names))

        # Rodar o calc.raster usando esse ano específico como uma coluna
        raster.data.proxy <- calc.raster(geo.file = mesh.geo.file,
                                         tif.file = x.int[[1]],
                                         year.used = year.proxy)

        return(raster.data.proxy)
      })

    # Change the list to a single data.frame with all the columns
    raster.data <- dplyr::bind_rows(raster.data)
    raster.data <- raster.data[order(raster.data$ID_mesh),]

    # If the output doesnt exist, create it
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


    # Return ####
    return(raster.data.km2.growth)
  }
