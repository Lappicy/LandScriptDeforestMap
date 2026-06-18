# Complete function (Growth.Analysis) ####
Growth.Analysis <-
  function(geo.file, tif.folder,
           mesh.size,
           output.folder, output.name,
           MAPBIOMAS = NULL, PRODES = NULL, ...){

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
