\name{count.classes}
\alias{count.classes}
\title{Define classes from numbers, following MapBiomas or PRODES key.}
\usage{
count.classes(proxy.table, MAPBIOMAS = NULL, PRODES = NULL,
              num.forest = NULL, num.non.forest = NULL,
              num.urban = NULL, num.mining = NULL,
              num.pasture = NULL, num.agriculture = NULL,
              num.water = NULL, num.others = NULL)
}
\arguments{
  \item{proxy.table}{Table with the correct format (having columns names as the legend key from MapBiomas or PRODES data).}
  \item{MAPBIOMAS}{Which version of MapBiomas you are using (versions 4, 7.1 and 8 supported).}
  \item{PRODES}{Not yet available.}
  \item{num.forest}{Which numbers represent "forest".}
  \item{num.non.forest}{Which numbers do not represent "forest".}
  \item{num.urban}{Which numbers represent "urban".}
  \item{num.mining}{Which numbers represent "mining".}
  \item{num.pasture}{Which numbers represent "pasture".}
  \item{num.agriculture}{Which numbers represent "agriculture".}
  \item{num.water}{Which numbers represent "water".}
  \item{num.others}{Which numbers represent "others".}
}
\value{
This function returns a table that has a named column for the 8 different classes proposed in the function (forest, non-forest, urban, mining, pasture, agriculture, water and others).
}
\description{
This function reframes the table given \code{proxy.table}, to merge the different numbers that belongs to a same class, one of the 8 used by the function (forest, non-forest, urban, mining, pasture, agriculture, water and others). The table still maintains the individual columns with numerical names.
}
\examples{
# No practical example for this function yet
}
