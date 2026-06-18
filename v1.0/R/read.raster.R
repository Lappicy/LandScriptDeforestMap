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
