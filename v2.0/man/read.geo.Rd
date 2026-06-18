\name{read.geo}
\alias{read.geo}
\title{Read a vector (geospatial) file and makes it the wanted projection.}
\usage{
read.geo(file.name, projection.wanted = 4326)
}
\arguments{
  \item{file.name}{The directory which to open the desired vector / geospatial data, or the object itself}
  \item{projection.wanted}{The CRS code for the desired projection}
}
\value{
This function returns a sf object with the geometry column named as "geometry".
}
\description{
This function reads the vector (spatial) file given if it is not in a format opened in R already. It then reprojects the data to the desired projection defined by \code{projection.wanted}. It then changes the name for the geometry column as "geometry".
}
\examples{
example.files()

# There is no problem in using the function in an object in the right format already
CavernaMaroaga <- read.geo(CavernaMaroaga)
}
