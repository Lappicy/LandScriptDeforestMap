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
