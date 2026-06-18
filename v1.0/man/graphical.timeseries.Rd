\name{graphical.timeseries}
\alias{graphical.timeseries}
\title{Graph for deforestation with other classes and highest correlation.}
\usage{
graphical.timeseries(proxy.table,
                     comparison.name = "Growth",
                     comparison.color = c("purple", "grey50", "#EA9999", "darkorange"),
                     save.as = "Results/Deforestation vs Growth.png",
                     title.name = "Analysis for Caverna do Maroaga",
                     different.groups = NULL,
                     ...)
}
\arguments{
  \item{proxy.table}{Table with the correct format, having columns with the names from \code{type.one} and \code{type.two}.}
  \item{comparison.name}{Which is the first class analysed, used to compare to deforestation usually.}
  \item{comparison.color}{Color pallete to be used with the \code{comparison.name} data.}
  \item{save.as}{Where to save the image and format.}
  \item{title.name}{Name of title to appear on the plot.}
  \item{different.groups}{Which different groups should be used to separate the images. One may choose to make this analysis for specific subgroup from the table given at \code{proxy.table}}
  \item{...}{Other arguments passed on to the \code{ggplot()} used. For now, it accepts the arguments for "width", "height", "units" and "dpi" only.}
}
\value{
This function creates a graph from a given table or sf object. It also saves the graph in the \code{save.as} folder and name.
}
\description{
The function takes a table as input, \code{proxy.table}, needing to have columns defined by the \code{comparison.name} argument. These columns names may be just the first initial letters of the entire name (e.g. "Growth_" representing "Growth_urban", "Growth_mining"...). It also analyses the pearson correlation between the \code{comparison.name} and \code{Deforestation} columns, printing the highest correlation found between these two groups. It then creates a ggplot2 graph for the time series given, also writing the highest correlation found.
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
graphical.timeseries(proxy.table = FinalAnalysis,
                     comparison.name = "Growth",
                     comparison.color = c("purple", "grey50", "#EA9999", "darkorange"),
                     save.as = "Results/Deforestation vs Growth.png",
                     title.name = "Analysis for Caverna do Maroaga")
}
