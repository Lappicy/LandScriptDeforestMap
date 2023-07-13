# Complete function (Growth.Analysis) ####
Growth.Analysis <-
  function(geo.file, tif.folder,
           mesh.size,
           output.folder, output.name,
           MAPBIOMAS = NULL, PRODES = NULL, ...){

    # Functions used ####
    {
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
      # Create a mesh for a component (only squares supported for now) ####
      create.mesh <- function(geo.file,
                              mesh.size = 0.25,
                              mesh.format = "square"){

        # Libraries used ####
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
        sf::st_geometry(mesh.map) <- "geom"


        # Return ####
        # Returns the mesh.map where it intersects with the geo.file with ID column
        return(mesh.map)
      }
      # Based on a geospatial file, sum all classification of a raster ####
      calc.raster <- function(geo.file, tif.file, year.used = NULL,
                              folder.output = "Proxy/"){

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

          # crop the tif.file and create a mask for it
          tif.crop <- raster::crop(x = tif.file, y = geo.file[i.calc.raster,])
          tif.mask <- raster::mask(x = tif.crop, mask = geo.file[i.calc.raster,])

          # Turn the raster into a matrix and get a table with summary data
          tif.matrix <- raster::as.matrix(tif.mask)
          tif.classes <- base::table(as.vector(tif.matrix))

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

            # if the number is less than 10, add three zeros
            # if the number is less than 100, add two zeros
            # if the number is less than 1000, add one zero
            name.calc.raster <-
              ifelse(i.calc.raster < 10, paste0("000", i.calc.raster),
                     ifelse(i.calc.raster < 100, paste0("00", i.calc.raster),
                            ifelse(i.calc.raster < 1000, paste0("0", i.calc.raster),
                                   i.calc.raster)))

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
                                num.others = NULL){

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

        # Function ifself ####
        # Organize data ####
        # Number of diferent columns used
        n.types.table <- unique(proxy.table[,column.used])

        # Create empty columns
        proxy.table$Deforestation <- NA
        proxy.table$Reforestation <- NA
        proxy.table$Growth_Agriculture <- NA
        proxy.table$Growth_Mining <- NA
        proxy.table$Growth_Pasture <- NA
        proxy.table$Growth_Urban <- NA

        # Columns that we want to use later
        suppressWarnings({
          col.num <- which(!is.na(as.numeric(colnames(proxy.table))))
        })

        first.col <- 1:(min(col.num) - 1)

        # Reorder columns
        proxy.table <- proxy.table[, c(colnames(proxy.table)[first.col],
                                       "Deforestation", "Reforestation",
                                       "Growth_Urban", "Growth_Mining",
                                       "Growth_Pasture", "Growth_Agriculture",
                                       "Forest", "NonForest", "Water", "Others",
                                       "Urban", "Mining", "Pasture", "Agriculture",
                                       sort(colnames(proxy.table)[col.num]))]


        # Calculate growth ####
        # DEFORESTATION
        proxy.table[2:nrow(proxy.table), "Deforestation"] <-
          base::diff(proxy.table[, "Forest"], lag = 1) * (-1)

        # REFORESTATION (when deforestation is negative)
        proxy.table[, "Reforestation"] <-
          ifelse(is.na(proxy.table[, "Deforestation"]) |
                   proxy.table[, "Deforestation"] < 0,
                 proxy.table[, "Deforestation"] * (-1),
                 0)

        # GROWTH AGRICULTURE
        proxy.table[2:nrow(proxy.table), "Growth_Agriculture"] <-
          base::diff(proxy.table[, "Agriculture"], lag = 1)

        # GROWTH MINING
        proxy.table[2:nrow(proxy.table), "Growth_Mining"] <-
          base::diff(proxy.table[, "Mining"], lag = 1)

        # GROWTH PASTURE
        proxy.table[2:nrow(proxy.table), "Growth_Pasture"] <-
          base::diff(proxy.table[, "Pasture"], lag = 1)

        # GROWTH URBAN
        proxy.table[2:nrow(proxy.table), "Growth_Urban"] <-
          base::diff(proxy.table[, "Urban"], lag = 1)

        # Primeiro ano sempre NA
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
    }


    # Function itself ####
    # Read geo.file
    geo.file <- read.geo(file.name = geo.file)

    # Create mesh
    mesh.geo.file <- create.mesh(geo.file = geo.file, mesh.size = mesh.size)

    # Get the complete files names where the tif.files are located (tif.folder)
    tif.files.names <- list.files(tif.folder, full.names = TRUE)

    # Calculate the classes for each raster file
    for(i in 1:length(tif.files.names)){

      year.proxy <- substr(tif.files.names[i],
                           nchar(tif.files.names[i]) - 7,
                           nchar(tif.files.names[i]) - 4)

      raster.data.proxy <- calc.raster(geo.file = mesh.geo.file,
                                       tif.file = tif.files.names[i],
                                       year.used = year.proxy)

      if(i == 1) raster.data <- raster.data.proxy
      if(i > 1) raster.data <- base::merge(raster.data, raster.data.proxy,
                                           all = TRUE)

      remove(year.proxy, raster.data.proxy)
    }

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