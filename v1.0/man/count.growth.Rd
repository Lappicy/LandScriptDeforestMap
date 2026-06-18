\name{count.growth}
\alias{count.growth}
\title{Count yearly growth for the different classes.}
\usage{
count.grouwth(proxy.table, output.folder, output.name, column.used = NULL)
}
\arguments{
  \item{proxy.table}{Table (or sf object) with the correct format (having named columns for 8 classes - see \code{count.classes} function).}
  \item{output.folder}{Which folder to save the output files.}
  \item{output.name}{Name of the file to be exported.}
  \item{column.used}{Which columns have the data on which to calculate yearly growth. Default is NULL that uses the 8 classes from the output of the \code{count.classes} function. They are "Forest", "NonForest", "Urban", "Mining", "Pasture", "Agriculture", "Water" and "Others")}
}
\value{
This function returns a table with "Growth_" columns for different classes ("Deforestation", "Reforestation", "Growth_Agriculture", "Growth_Mining", "Growth_Pasture" and "Growth_Urban"). This column shows the growth in the number of pixels when compared to the previous year.
}
\description{
This function creates 6 new columns on the input table, or sf object, ("Deforestation", "Reforestation", "Growth_Agriculture", "Growth_Mining", "Growth_Pasture" and "Growth_Urban"), \code{proxy.table}. Then the yearly growth of each class is calculated, with negative values being changed to 0 (no negative growth is considered). Two tables are saved, one having all the data and the other having only the growth and named columns. In addition, two vector files are also saved if the input is a sf object.
}
\examples{
# No practical example for this function only yet
}
