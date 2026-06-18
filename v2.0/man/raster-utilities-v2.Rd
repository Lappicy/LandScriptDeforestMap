\name{raster.Classes}
\alias{raster.Classes}
\alias{SankeyDiagram}
\title{Additional raster utilities}
\usage{
raster.Classes(tif.files, classes, class.name, output.name)

SankeyDiagram(tif1 = NULL, tif2 = NULL)
}
\arguments{
  \item{tif.files}{Raster file paths.}
  \item{classes}{Class codes included in the binary output.}
  \item{class.name}{Class label used in output file names.}
  \item{output.name}{Output file prefix.}
  \item{tif1, tif2}{Two classified rasters.}
}
\value{
\code{raster.Classes()} writes binary raster files and returns \code{NULL}.
\code{SankeyDiagram()} currently prepares the two raster inputs.
}
\description{
Utilities preserved from the version 2.0 scripts.
}
