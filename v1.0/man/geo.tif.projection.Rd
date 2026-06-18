\name{geo.tif.projection}
\alias{geo.tif.projection}
\title{Ensure the correct projection of vector (geospatial) file.}
\usage{
geo.tif.projection(geo.file, tif.file)
}
\arguments{
  \item{geo.file}{The vector / geospatial file used}
  \item{tif.file}{The image / raster used}
}
\value{
This function returns the \code{geo.file} given with the same projection as the \code{tif.file}.
}
\description{
This function reads the geospatial file with the existing \code{read.geo} function from this package. It then checks the projection of both the \code{geo.file} and the \code{tif.file}. If the projections are different, the function uses a \code{st_transform} from the \code{sf} package to transform the \code{geo.file} to the same projection as the \code{tif.file}, and return it.
}
\examples{
# Get example data
example.files()
MapBiomas.proxy <- MapBiomas_8_example[[1]]

# Make sure the projection is the same
geo.tif.projection(geo.file = CavernaMaroaga, tif.file = MapBiomas.proxy)
}
