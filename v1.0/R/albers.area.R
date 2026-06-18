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
