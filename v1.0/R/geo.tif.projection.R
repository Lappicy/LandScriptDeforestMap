# Ensure the correct projection of vector (geospatial) file ####
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
