\name{gg.deforestation.cor}
\alias{gg.deforestation.cor}
\alias{mesh.map}
\title{Create temporal graphics and thematic maps}
\usage{
gg.deforestation.cor(
  proxy.table,
  type.one = "Growth",
  type.two = "Deforestation",
  columns_one = NULL,
  columns_two = NULL,
  color.one = c("purple", "grey50", "#EA9999", "darkorange"),
  color.two = "darkgreen",
  save.as = "Results/Deforestation vs Growth.png",
  title.name = "Analysis for Uaça 1 e 2",
  different.groups = NULL
)

mesh.map(
  mesh.data,
  class = "Deforestation",
  year.used = "all",
  col.limits = c(0, 1, 2, 5),
  col.used = c("white", "#E5E200", "#FC780D", "red", "darkred"),
  save.map.as = NULL,
  map.height = 3000,
  map.width = 2000,
  map.units = "px",
  grid.color = "#17212B66",
  classes.column = NULL,
  classes.col = c("pink", "purple", "#E1AD01", "darkgreen", "lightgreen"),
  highlight = NULL
)
}
\arguments{
  \item{proxy.table}{Analysis table containing a \code{Year} column.}
  \item{type.one, type.two}{Patterns used to select compared columns.}
  \item{columns_one, columns_two}{Optional explicit column selections.}
  \item{color.one, color.two}{Colours used in the temporal graph.}
  \item{save.as}{Output path for the temporal graph.}
  \item{title.name}{Graph title.}
  \item{different.groups}{Optional grouping columns.}
  \item{mesh.data}{An analysis result containing spatial geometry.}
  \item{class}{Variable displayed on the map.}
  \item{year.used}{One or more years, or \code{"all"}.}
  \item{col.limits, col.used}{Class limits and fill colours.}
  \item{save.map.as}{Optional output path for the map.}
  \item{map.height, map.width, map.units}{Output dimensions and units.}
  \item{grid.color}{Colour of mesh borders.}
  \item{classes.column, classes.col, highlight}{Optional group highlighting settings.}
}
\value{
\code{gg.deforestation.cor()} saves a temporal graph and returns \code{NULL}.
\code{mesh.map()} returns a \code{ggplot} object and optionally saves it.
}
\description{
Functions for visualising temporal relations and spatial patterns from
LandScriptDeforestMap results.
}
