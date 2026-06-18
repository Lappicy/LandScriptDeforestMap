\name{read.raster}
\alias{read.raster}
\title{Read raster (image) file in R and stack it into a list.}
\usage{
read.raster(raster.file)
}
\arguments{
  \item{raster.file}{The raster file. It can be in a list format, RasterStack or character (showing the individual files in a list or showing the folder that has the raster files needed)}
}
\value{
This function returns a list object having for each element a raster.
}
\description{
This function reads the \code{raster.file} given and puts it into the same format: a list containing a raster for each element. This is so that the other functions may run properly.
}
\examples{
# Load data
example.files()

# There is no problem in using the function in an object in the right format already
list.of.raster <- read.raster(MapBiomas_8_example)
}
