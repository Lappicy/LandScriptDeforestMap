\name{Growth.Analysis}
\alias{Growth.Analysis}
\title{Entire growth analysis for time series of images using a mesh.}
\usage{
Growth.Analysis(geo.file, tif.folder, mesh.size,
                output.folder, output.name,
                MAPBIOMAS = NULL, PRODES = NULL, ...)
}
\arguments{
  \item{geo.file}{Vector / geospatial file containing the area on which the analysis should be done.}
  \item{tif.folder}{Folder which has the image files on your computes OR a list, or RasterStack, with the images to be used.}
  \item{mesh.size}{The refinement of the wanted mesh - for further information see \code{create.mesh()} function.}
  \item{output.folder}{Which folder to save the results.}
  \item{output.name}{Name of the file for the results.}
  \item{MAPBIOMAS}{Which version of MapBiomas you are using for the legend key.}
  \item{PRODES}{Which version of PRODES you are using for the legend key (not yet available).}
  \item{...}{Other information you might want to pass on (not yet available).}
}
\value{
This function was designed to group other functions from this package into one framework, thus making easier to use.
}
\description{
This function is an all englobing framework for the \code{DeforestMapBiomas} package. It reads the \code{geo.file=} with the \code{read.geo()} function, then it creates a mesh with the \code{create.mesh()}, with \code{mesh.size=} argument. For each image, it runs the \code{calc.raster()} function, calculating the number of pixels for each "ID_mesh" row created. It then transforms this values from pixels to km via the \code{pixel.to.km2()} function and then groups these columns with \code{count.classes()}. The yearly growth rate is later calculated by the \code{calc.growth()}. With the \code{output.folder=} and \code{output.name=}, four files are saved. Two having all the used data (in ".txt" and ".gpkg" format) and the other two being a simplified version, having only the columns that aren't numerical (both also in ".txt" and ".gpkg" format).
}
\examples{
# Load data
data("CavernaMaroaga")
data("MapBiomas_8_CavernaMaroaga")

# Run code
FinalAnalysis <-
  Growth.Analysis(geo.file = CavernaMaroaga,
                  tif.folder = MapBiomas_8_CavernaMaroaga,
                  mesh.size = 0.045,
                  output.folder = "Results/",
                  output.name = "Analysis_CavernaMaroaga.txt",
                  MAPBIOMAS = 8)
}
