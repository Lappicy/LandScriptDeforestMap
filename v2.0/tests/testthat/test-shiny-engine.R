test_that("example analysis exports the expected files and removes its proxy", {
  source_folder <- system.file("extdata", package = "LandScriptDeforestMap")
  raster_folder <- tempfile("example_rasters_")
  output_folder <- tempfile("example_output_")
  dir.create(raster_folder)
  dir.create(output_folder)

  raster_sources <- file.path(
    source_folder,
    c("MapBiomas_8_2013.tif", "MapBiomas_8_2014.tif")
  )
  expect_true(all(file.copy(raster_sources, raster_folder)))
  inspection <- inspect_raster_folder(raster_folder)

  params <- list(
    geo_path = file.path(source_folder, "CavernaMaroaga.gpkg"),
    raster_folder = raster_folder,
    raster_index = inspection$index,
    output_folder = output_folder,
    output_name = "automated_example",
    group_column = "Categoria",
    no_mesh = FALSE,
    mesh_size = 50000,
    mesh_unit = "meters",
    mapbiomas = "none",
    custom_classes = NULL,
    pixel_km2_ratio = inspection$pixel_area_km2,
    max_cells = 50000L,
    use_direct_paths = TRUE,
    resume_proxy = FALSE,
    validated_geo_rds = NULL
  )

  result <- run_land_analysis(params)
  expect_true(analysis_result_files_ready(result))
  expect_false(dir.exists(analysis_proxy_dir(output_folder, "automated_example")))
  expect_true(file.exists(result$files[["run_summary"]]))
  summary_text <- paste(readLines(result$files[["run_summary"]], encoding = "UTF-8"), collapse = "\n")
  expect_match(summary_text, "LandScriptDeforestMap - Resumo da execução", fixed = TRUE)
  expect_match(summary_text, "Quantidade de rasters: 2", fixed = TRUE)
  expect_match(summary_text, "Tamanho da malha: 50,000 km", fixed = TRUE)
  expect_match(summary_text, "Tempo por raster", fixed = TRUE)
  expect_match(summary_text, "MapBiomas_8_2013.tif", fixed = TRUE)
  expect_match(summary_text, "Tamanho total dos arquivos listados", fixed = TRUE)
  expect_setequal(
    sf::st_layers(result$files[["complete_gpkg"]])$name,
    c("grid", "mesh", "groups", "total")
  )
  expect_setequal(
    readxl::excel_sheets(result$files[["complete_xlsx"]]),
    c("Grid_ID", "Malha", "Grupos", "Total")
  )
})

test_that("background job returns a lightweight pointer to the complete result", {
  source_folder <- system.file("extdata", package = "LandScriptDeforestMap")
  raster_folder <- tempfile("job_rasters_")
  output_folder <- tempfile("job_output_")
  dir.create(raster_folder)
  dir.create(output_folder)
  expect_true(file.copy(file.path(source_folder, "MapBiomas_8_2013.tif"), raster_folder))
  inspection <- inspect_raster_folder(raster_folder)

  params <- list(
    geo_path = file.path(source_folder, "CavernaMaroaga.gpkg"),
    raster_folder = raster_folder,
    raster_index = inspection$index,
    output_folder = output_folder,
    output_name = "background_example",
    group_column = "Categoria",
    no_mesh = TRUE,
    mesh_size = NULL,
    mesh_unit = "meters",
    mapbiomas = "none",
    custom_classes = NULL,
    pixel_km2_ratio = inspection$pixel_area_km2,
    max_cells = 50000L,
    use_direct_paths = TRUE,
    resume_proxy = FALSE,
    validated_geo_rds = NULL
  )
  progress_file <- file.path(output_folder, "progress.json")
  job_file <- file.path(output_folder, "job.rds")

  run_analysis_job(params, progress_file, job_file, landscript_test_app_dir)
  pointer <- readRDS(job_file)
  lazy_result <- read_analysis_job_result(job_file, lazy = TRUE)
  result <- read_analysis_job_result(job_file)

  expect_s3_class(pointer, "landscript_lazy_result")
  expect_true(is_lazy_analysis_result(lazy_result))
  expect_setequal(analysis_result_levels(lazy_result), c("grid", "mesh", "groups", "total"))
  expect_lt(file.info(job_file)$size, 8192)
  lazy_mesh <- load_analysis_result_level(lazy_result, "mesh")
  expect_s3_class(lazy_mesh, "sf")
  expect_equal(nrow(lazy_mesh), nrow(result$mesh))
  expect_identical(load_analysis_result_level(lazy_result, "mesh"), lazy_mesh)
  expect_identical(ls(lazy_result$cache), "mesh")
  expect_true(analysis_result_files_ready(result))
  expect_true(analysis_result_files_ready(lazy_result))
  expect_equal(read_progress(progress_file)$status, "complete")
})

test_that("lazy result loading supports split GeoPackages, including the mesh", {
  folder <- tempfile("split_lazy_")
  dir.create(folder)
  geometry <- sf::st_as_sfc(
    sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 1, ymax = 1), crs = sf::st_crs(4326))
  )
  layer <- sf::st_sf(
    ID_mesh = "M1",
    Grid_ID = "G1",
    Year = 2020L,
    geometry = geometry
  )
  layers <- list(grid = layer, mesh = layer, groups = layer, total = layer)
  files <- write_result_geopackages(folder, "split", layers, threshold_bytes = 1)
  complete <- c(
    layers,
    list(
      raster_index = data.frame(file = "classes_2020.tif", year = 2020L),
      group_column = character(),
      output_name = "split",
      output_folder = folder,
      overlap_removed = FALSE,
      files = files
    )
  )
  lazy <- analysis_result_pointer(complete)
  lazy$cache <- new.env(parent = emptyenv())

  expect_false("complete_gpkg" %in% names(files))
  expect_s3_class(load_analysis_result_level(lazy, "mesh"), "sf")
  expect_s3_class(load_analysis_result_level(lazy, "groups"), "sf")
  expect_setequal(ls(lazy$cache), c("groups", "mesh"))
})

test_that("MapBiomas Collection 11 legend follows the supplied class codes", {
  collection_11_codes <- c(
    3, 6, 4, 7, 5, 49, 12, 77, 11, 84, 50, 32, 29, 15,
    39, 20, 40, 62, 41, 46, 47, 35, 48, 9, 21, 23, 24, 30,
    75, 91, 25, 33, 31
  )
  mapping <- mapbiomas_class_map("11")
  primary_codes <- sort(unique(unlist(mapping[c("Forest", "NonForest", "Water")])))

  expect_setequal(primary_codes, collection_11_codes)
  expect_identical(mapping$Forest, 3)
  expect_identical(mapping$Water, c(31, 33))
  expect_true(all(c("Urban", "Mining", "Pasture", "Agriculture") %in% names(mapping)))

  input <- as.data.frame(
    matrix(
      seq_along(collection_11_codes),
      nrow = 1L,
      dimnames = list(NULL, as.character(collection_11_codes))
    ),
    check.names = FALSE
  )
  classified <- count.classes(input, MAPBIOMAS = "11")

  expect_equal(classified$Forest, input[["3"]])
  expect_equal(classified$Water, input[["31"]] + input[["33"]])
  expect_equal(classified$Urban, input[["24"]])
  expect_equal(classified$Mining, input[["30"]])
  expect_equal(classified$Pasture, input[["15"]])
})
