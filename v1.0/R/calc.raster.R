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
