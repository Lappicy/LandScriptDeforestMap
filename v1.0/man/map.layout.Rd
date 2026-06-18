\name{map.layout}
\alias{map.layout}
\title{Map for the mesh results given by the \code{Growth.Analysis()} function}
\usage{
map.layout(mesh.data,
           class = "Deforestation",
           year.used = "all",
           col.limits = c(0, 1, 2, 5),
           col.used = c("white", "#E5E200", "#FC780D", "red", "darkred"),
           save.map.as,
           ...)
}
\arguments{
  \item{mesh.data}{Table containing information about a given year and class. The desired format is the same as tue output from the \code{Growth.Analysis()} function.}
  \item{class}{Which class you want the map to be about (column name from the \code{mesh.data=} argument)}
  \item{year.used}{Which year you want to analyze. If \code{year.used = "all"} or more than one year, then the acumulated growth of each class is presented on the map.}
  \item{col.limits}{Which are the limits to group the class chosen.}
  \item{col.used}{Which colors should be used for the limits proposed in \code{col.limits=} argument.}
  \item{save.map.as}{Name to save this map as, with the format at the end (e.g. ".png" or ".pdf").}
  \item{...}{Other arguments passed on to the \code{ggplot()} used. For now, it accepts the arguments for "width", "height", "units" and "dpi" only.}
}
\value{
This function creates a map from the output of the \code{Growth.Analysis()} function. It is a gridded map showing different degrees of different classes for a given year or the acumulated growth value for such class.
}
\description{
This function takes the output from the \code{Growth.Analysis()} function, selecting only the \code{class=} column to be analysed. The function then groups these values for the \code{year.used=} argument, grouping the growth rate when the \code{year.used=} is bigger than one. It then groups these classes values for the \code{col.limits=} defined and creates a ggplot2 object. This object is also saved given the \code{save.map.as=}.
}
\examples{
# Load data
example.files()

# Run previous analysis needed (takes about ~10min)
FinalAnalysis <-
  Growth.Analysis(geo.file = CavernaMaroaga,
                  tif.folder = MapBiomas_8_example,
                  mesh.size = 0.045,
                  output.folder = "Results/",
                  output.name = "Analysis_CavernaMaroaga.txt",
                  MAPBIOMAS = 8)

# Run this function code
map.layout(mesh.data = FinalAnalysis,
           save.map.as = "Results/Acumulated deforestation map.png")
}
