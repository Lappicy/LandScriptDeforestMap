\name{calc.raster}
\alias{calc.raster}
\title{Calculates how many pixels have the same number for each row in a file.}
\usage{
calc.raster(geo.file, tif.file, year.used = NULL, folder.output = "Proxy/")
}
\arguments{
  \item{geo.file}{Vector file in sf, .gpkg, or .shp format (also known as GIS or geospatial data).}
  \item{tif.file}{Image file in a raster format.}
  \item{year.used}{Which specific year the \code{tif.file} given is for. If not known, the default is NULL.}
  \item{folder.output}{Folder on which R will save the files for each \code{ID_mesh} and \code{year}. The default is a temporary folder called "\code{Proxy/}", which is later deleted.}
}
\value{
This function returns a sf object that resembles the \code{geo.file} object but with extra columns. These columns are numerical, and have numerical names. They contain the ammount of pixels for such class (column name = class) for the geometry present in that row.
}
\description{
This function reads the \code{geo.file} with the existing \code{read.geo} function from this package. For each row in the \code{geo.file} object, the function will crop and create a mask on the \code{tif.file} image. It will turn this image into a matrix and calculate the ammount of times each numerical pixel appears. Each time the loop runs through a line (done through an apply function), a file is saved in the \code{folder.output}. When the loop ends, a file with all the rows is created. New columns are created with the pixel number from the classification and organized in ascending order.
}
\examples{
# No practical example for this function yet
}
