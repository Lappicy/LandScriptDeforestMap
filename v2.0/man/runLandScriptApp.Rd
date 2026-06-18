\name{runLandScriptApp}
\alias{runLandScriptApp}
\title{Run the LandScriptDeforestMap Shiny application}
\usage{
runLandScriptApp(launch.browser = interactive(), ...)
}
\arguments{
  \item{launch.browser}{Open the application in the default browser.}
  \item{...}{Additional arguments passed to \code{shiny::runApp()}.}
}
\value{
The value returned invisibly by \code{shiny::runApp()}.
}
\description{
Copies the application bundled with version 2.0 to a writable temporary
directory and starts it.
}
\examples{
\dontrun{
runLandScriptApp()
}
}
