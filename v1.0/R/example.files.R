# Download the files existed in the example application ####
example.files <- function(){

  # Dependencies ####
  require(sf)
  require(raster)


  # Function itself ####
  # Create a list having all the MapBiomas data in inst/extdata
  MapBiomas_8_example <<-
    list(raster::raster(system.file("extdata", "MapBiomas_8_2013.tif", package = "LandScriptDeforestMap")),
         raster::raster(system.file("extdata", "MapBiomas_8_2014.tif", package = "LandScriptDeforestMap")),
         raster::raster(system.file("extdata", "MapBiomas_8_2015.tif", package = "LandScriptDeforestMap")),
         raster::raster(system.file("extdata", "MapBiomas_8_2016.tif", package = "LandScriptDeforestMap")),
         raster::raster(system.file("extdata", "MapBiomas_8_2017.tif", package = "LandScriptDeforestMap")),
         raster::raster(system.file("extdata", "MapBiomas_8_2018.tif", package = "LandScriptDeforestMap")),
         raster::raster(system.file("extdata", "MapBiomas_8_2019.tif", package = "LandScriptDeforestMap")),
         raster::raster(system.file("extdata", "MapBiomas_8_2020.tif", package = "LandScriptDeforestMap")),
         raster::raster(system.file("extdata", "MapBiomas_8_2021.tif", package = "LandScriptDeforestMap")),
         raster::raster(system.file("extdata", "MapBiomas_8_2022.tif", package = "LandScriptDeforestMap")))

  # Create the vector file having the studied area
  CavernaMaroaga <<-
    sf::st_read(system.file("extdata", "CavernaMaroaga.gpkg", package = "LandScriptDeforestMap"))


  # Returns nothing ####
  return()
}
