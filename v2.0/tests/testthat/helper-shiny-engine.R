landscript_test_app_dir <- system.file(
  "shiny",
  "LandScriptDeforestMap",
  package = "LandScriptDeforestMap"
)

if (!nzchar(landscript_test_app_dir)) {
  stop("A aplicação Shiny instalada não foi encontrada para os testes.")
}

options(landscript.app_dir = landscript_test_app_dir)
source(file.path(landscript_test_app_dir, "R", "utils.R"), local = TRUE)
source(file.path(landscript_test_app_dir, "R", "spatial_io.R"), local = TRUE)
source(file.path(landscript_test_app_dir, "R", "landscript_engine.R"), local = TRUE)
source(file.path(landscript_test_app_dir, "R", "plots.R"), local = TRUE)
