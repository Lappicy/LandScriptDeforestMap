\name{albers.area}
\alias{albers.area}
\alias{Albers.Area}
\title{Calculate polygon areas using an Albers equal-area projection}
\usage{
albers.area(geo.file)

Albers.Area(geo.file)
}
\arguments{
  \item{geo.file}{An \code{sf} polygon object.}
}
\value{
The input object with an \code{Area_km2} column.
}
\description{
Calculates polygon areas in square kilometres using a South America Albers
equal-area projection. Both names call the same implementation.
}
