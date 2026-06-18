\name{close.geo.connections}
\alias{close.geo.connections}
\title{Closes the connections made using the functinos of these packages.}
\usage{
close.geo.connections(OS = "Windows")
}
\arguments{
  \item{OS}{The operating system of your computer (only "WINDOWS" is supported for now)}
}
\value{
This function returns nothing.
}
\description{
This function closes all the connections made, with the \code{closeAllConnections()} function. It also \code{unlinks} all the temp folders created. It is designed to alliviate the RAM being used throughout the functions.
}
\examples{
# Usually used inside bigger functions
close.geo.connections()
}
