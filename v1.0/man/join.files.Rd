\name{join.files}
\alias{join.files}
\title{Join files from a directory and then deletes such directory.}
\usage{
join.files(folder.calc.raster, new.folder,
           output.name, column.order = NULL)
}
\arguments{
  \item{folder.calc.raster}{Folder on which to merge all the files inside. This is used inside the \code{Growth.Analysis()} function, used to merge all the files created by the \code{calc.raster()} function, named "Proxy/".}
  \item{new.folder}{Where you want the new joined file to be located.}
  \item{output.name}{Name of the new joined file.}
  \item{column.order}{The order on which you want the final data.frame to be in. The default is NULL as the function will try to put the numerical column names in ascending order.}
}
\value{
Returns a table with all the files from the \code{folder.calc.raster=} merged into one single data.frame.
}
\description{
This function lists all the files inside the \code{folder.calc.raster=} directory, then it reads all such files and bind the rows together. This new file is returned as a data.frame and saved on the \code{new.folder=} location with the \code{output.name=} given.
}
\examples{
# No examples for this functions were created since it is usually used inside other functions.
}
