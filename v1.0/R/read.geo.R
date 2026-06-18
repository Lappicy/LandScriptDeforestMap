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
