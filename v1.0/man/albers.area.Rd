\name{albers.area}
\alias{albers.area}
\title{Calculate the area of a SF object with the Albers projection}
\usage{
albers.area(geo.file)
}
\arguments{
  \item{geo.file}{A vector (geo spatial or GIS data) file (.gpkg, .shp, sf file...)}
}
\value{
This function returns the same \code{geo.file} given as input with an extra column, named "\code{Area_km2}", which represents the area of such file with the Albers projection.
}
\description{
This function uses the \code{read.geo} to read the spatial file given if it is not in a format opened in R already. It then transformrs the crs from the file to the Albers projection. It calculates the area (in km2) and then creates a column in the original file, \code{geo.file}, with this area. Note that the outputed file remains in the same projection as the original.
}
\examples{
example.files()

CavernaMaroaga <- albers.area(CavernaMaroaga)
}
