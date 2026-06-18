\name{pixel.to.km2}
\alias{pixel.to.km2}
\title{Changes pixel values to km2.}
\usage{
pixel.to.km2(proxy.table, pixel.km2.ratio = 30*30/1000000)
}
\arguments{
  \item{proxy.table}{The data.frame file with the pixel columns with numeric names.}
  \item{pixel.km2.ration}{The ratio used for the transformation. The default is for the Landsite satellites which have a 30m resolution.}
}
\value{
This function returns the same \code{proxy.table} given but with the pixel columns (numeric) passed through the ration defined at \code{pixel.km2.ratio}.
}
\description{
This function checks which column names from the \code{proxy.table} are numeric. The function defines all numerical named columns as being pixel variables. It then transforms the data by the \code{pixel.km2.ratio} defined, and returns this table with new values.
}
\examples{
# Create data.frame with one column as being numeric
df.example <- data.frame(Name = rep("names", 3),
                         "1" = c(20000, 0, 10000),
                         X1 = c(20000, 0, 10000),
                         Pixel_1 = c(20000, 0, 10000),
                         check.names = FALSE)

# Only the second column, named "1", will have its value multiplied by the pixel.km2.ratio
df.example <- pixel.to.km2(df.example)
}
