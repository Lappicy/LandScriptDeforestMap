#' Run the LandScriptDeforestMap Shiny application
#'
#' Copies the bundled application to a writable temporary directory and starts
#' it with [shiny::runApp()].
#'
#' @param launch.browser Open the application in the default browser.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The value returned invisibly by [shiny::runApp()].
#' @export
runLandScriptApp <- function(launch.browser = interactive(), ...) {
  app_source <- system.file(
    "shiny",
    "LandScriptDeforestMap",
    package = "LandScriptDeforestMap"
  )

  if (!nzchar(app_source) || !dir.exists(app_source)) {
    stop("A aplicação Shiny não foi encontrada na instalação do pacote.", call. = FALSE)
  }

  app_runtime <- tempfile("LandScriptDeforestMap-Shiny-")
  dir.create(app_runtime, recursive = TRUE, showWarnings = FALSE)

  files <- list.files(
    app_source,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
  copied <- file.copy(
    files,
    app_runtime,
    recursive = TRUE,
    overwrite = TRUE,
    copy.mode = TRUE
  )
  if (!all(copied)) {
    stop("Não foi possível preparar a aplicação Shiny temporária.", call. = FALSE)
  }

  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(app_runtime)

  shiny::runApp(
    appDir = app_runtime,
    launch.browser = launch.browser,
    ...
  )
}
