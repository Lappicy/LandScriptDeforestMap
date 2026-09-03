test_that("pixel area is calculated from one cell without changing the result", {
  raster <- terra::rast(
    nrows = 12,
    ncols = 16,
    xmin = -2,
    xmax = 2,
    ymin = -1,
    ymax = 1,
    crs = "EPSG:4326"
  )
  center_cell <- terra::cellFromXY(raster, matrix(c(0, 0), ncol = 2))[[1]]
  expected <- terra::values(terra::cellSize(raster, unit = "km"))[[center_cell]]

  expect_equal(
    raster_pixel_area_from_raster(raster),
    expected,
    tolerance = 1e-12
  )
})

test_that("macOS AppleDouble files are ignored while indexing rasters", {
  folder <- tempfile("raster_index_")
  dir.create(folder)
  raster <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
  terra::values(raster) <- 1:4
  terra::writeRaster(raster, file.path(folder, "classified_2020.tif"), overwrite = TRUE)
  writeLines("metadata", file.path(folder, "._classified_2020.tif"))

  index <- list_raster_files(folder)
  expect_equal(nrow(index), 1L)
  expect_equal(index$year, 2020L)
  expect_equal(index$file, "classified_2020.tif")
})

test_that("local rasters stay in place when only the geospatial input needs proxying", {
  source_folder <- system.file("extdata", package = "LandScriptDeforestMap")
  raster_folder <- tempfile("direct_rasters_")
  proxy_folder <- tempfile("selective_proxy_")
  dir.create(raster_folder)
  dir.create(proxy_folder)

  source_raster <- file.path(source_folder, "MapBiomas_8_2013.tif")
  local_raster <- file.path(raster_folder, basename(source_raster))
  expect_true(file.copy(source_raster, local_raster))
  inspection <- inspect_raster_folder(raster_folder)
  source_geo <- file.path(source_folder, "CavernaMaroaga.gpkg")

  prepared <- prepare_proxy_inputs(
    list(
      geo_path = source_geo,
      raster_folder = raster_folder,
      raster_index = inspection$index,
      use_direct_paths = FALSE,
      use_direct_geo_path = FALSE,
      use_direct_raster_path = TRUE
    ),
    proxy_folder
  )

  expect_identical(
    normalizePath(prepared$raster_index$path, mustWork = TRUE),
    normalizePath(local_raster, mustWork = TRUE)
  )
  expect_identical(
    normalizePath(prepared$raster_folder, mustWork = TRUE),
    normalizePath(raster_folder, mustWork = TRUE)
  )
  expect_true(startsWith(prepared$geo_path, normalizePath(proxy_folder, mustWork = TRUE)))
  expect_false(dir.exists(file.path(proxy_folder, "inputs", "rasters")))
})

test_that("disk-space advisory warns but explicitly allows execution to continue", {
  advisory <- disk_space_advisory(
    list(estimated_uncompressed_zone_bytes = 1024^5),
    tempdir()
  )
  skip_if(is.null(advisory), "Available disk space could not be detected on this platform")

  expect_match(advisory$text, "Espaço em disco possivelmente insuficiente", fixed = TRUE)
  expect_match(advisory$text, "A execução continuará", fixed = TRUE)
  expect_gt(advisory$needed_bytes, advisory$available_bytes)
})

test_that("processing ETA is conservative and uses one decimal place", {
  estimate <- estimate_remaining_processing_seconds(
    completed_raster_seconds = 300,
    remaining_rasters = 9
  )

  expect_equal(estimate, 3270)
  expect_identical(format_processing_time(estimate), "54,5 min")
  expect_identical(format_processing_time(59 * 60), "59,0 min")
  expect_identical(format_processing_time(60 * 60), "1,0 h")
  expect_null(format_processing_time(NA_real_))
  expect_identical(format_summary_duration(0), "0 s")
  expect_identical(format_summary_duration(3661), "1 h 1 min 1 s")
})

test_that("progress files carry ETA state for the dashboard", {
  progress_file <- tempfile("progress_eta_", fileext = ".json")
  write_progress(
    progress_file,
    25,
    "Raster 1 de 40",
    "Primeiro raster concluído",
    eta_status = "estimated",
    eta_seconds = 7200
  )
  progress <- read_progress(progress_file)

  expect_identical(progress$eta_status, "estimated")
  expect_equal(progress$eta_seconds, 7200)
})

test_that("spherical self-crossings are repaired without losing result rows", {
  crossing <- matrix(
    c(0, 0, 1, 1, 0, 1, 1, 0, 0, 0),
    ncol = 2,
    byrow = TRUE
  )
  invalid <- sf::st_sf(
    Grid_ID = 1L,
    geometry = sf::st_sfc(sf::st_polygon(list(crossing)), crs = 4326)
  )

  expect_false(suppressWarnings(sf::st_is_valid(invalid))[[1]])
  repaired <- ensure_valid_polygon_geometry(invalid, "teste", preserve_rows = TRUE)
  expect_equal(nrow(repaired), nrow(invalid))
  expect_true(all(suppressWarnings(sf::st_is_valid(repaired))))
})

test_that("map rendering repairs old invalid result geometries", {
  crossing <- matrix(
    c(0, 0, 1, 1, 0, 1, 1, 0, 0, 0),
    ncol = 2,
    byrow = TRUE
  )
  geometry <- sf::st_sfc(sf::st_polygon(list(crossing)), crs = 4326)
  result <- sf::st_sf(
    Grid_ID = c(1L, 1L),
    Year = c(2020L, 2021L),
    Deforestation = c(1, 2),
    geometry = c(geometry, geometry)
  )

  map <- mesh.map(
    result,
    class = "Deforestation",
    year.used = "all",
    satellite = FALSE
  )
  expect_s3_class(map, "ggplot")
  expect_silent(ggplot2::ggplot_build(map))
})

test_that("result maps show the square mesh dimensions in a separate legend", {
  metric_grid <- sf::st_sf(
    Grid_ID = 1:4,
    Year = rep(2020L, 4),
    Deforestation = c(0, 1, 2, 3),
    geometry = sf::st_make_grid(
      sf::st_as_sfc(
        sf::st_bbox(
          c(xmin = 0, ymin = 0, xmax = 10000, ymax = 10000),
          crs = sf::st_crs(31983)
        )
      ),
      cellsize = 5000
    )
  )
  geographic_grid <- sf::st_transform(metric_grid, 4326)

  inferred <- infer_mesh_dimensions_km(geographic_grid)
  expect_equal(unname(inferred), c(5, 5), tolerance = 0.02)

  map <- mesh.map(
    geographic_grid,
    class = "Deforestation",
    year.used = 2020,
    satellite = FALSE,
    mesh.size.km = 5
  )
  shape_scale <- map$scales$get_scales("shape")

  expect_identical(shape_scale$name, "Tamanho da malha")
  expect_identical(shape_scale$get_labels(), "5 × 5 km")
  mesh_legend_layer <- Filter(
    function(layer) "shape" %in% names(layer$mapping),
    map$layers
  )
  expect_length(mesh_legend_layer, 1L)
  expect_false(unname(mesh_legend_layer[[1]]$show.legend[["fill"]]))
  expect_silent(ggplot2::ggplot_build(map))
})

test_that("Esri background opacity is passed independently to the map tile", {
  geometry <- sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(-48, -16, -47, -16, -47, -15, -48, -15, -48, -16),
      ncol = 2,
      byrow = TRUE
    ))),
    crs = 4326
  )
  result <- sf::st_sf(
    Grid_ID = 1L,
    Year = 2020L,
    Deforestation = 1,
    geometry = geometry
  )

  map <- mesh.map(
    result,
    class = "Deforestation",
    year.used = 2020,
    satellite = TRUE,
    satellite.alpha = 0.35,
    fill.alpha = 0.8,
    mesh.size.km = NA_real_
  )

  expect_equal(map$layers[[1]]$geom_params$alpha, 0.35)
  class_layer <- which(vapply(
    map$layers,
    function(layer) identical(layer$aes_params$alpha, 0.8),
    logical(1)
  ))
  expect_length(class_layer, 1L)
})

test_that("map unions fall back to planar GEOS for legacy invalid layers", {
  crossing <- matrix(
    c(0, 0, 1, 1, 0, 1, 1, 0, 0, 0),
    ncol = 2,
    byrow = TRUE
  )
  geometry <- sf::st_sfc(sf::st_polygon(list(crossing)), crs = 4326)
  legacy <- sf::st_sf(
    Year = c(2020L, 2021L),
    Deforestation = c(1, 2),
    geometry = c(geometry, geometry)
  )

  unioned <- safe_st_union(sf::st_geometry(legacy))
  expect_true(all(suppressWarnings(sf::st_is_valid(unioned))))

  map <- mesh.map(
    legacy,
    class = "Deforestation",
    year.used = "all",
    satellite = FALSE
  )
  expect_s3_class(map, "ggplot")
  expect_silent(ggplot2::ggplot_build(map))
})

test_that("temporal plot can hide only the correlation subtitle", {
  data <- data.frame(
    Year = 2020:2023,
    Deforestation = c(1, 3, 2, 5),
    Variation_Forest = c(2, 4, 1, 6)
  )
  with_correlation <- build_timeseries_plot(
    data,
    comparison.columns = "Variation_Forest",
    primary.column = "Deforestation",
    show.correlation = TRUE
  )
  without_correlation <- build_timeseries_plot(
    data,
    comparison.columns = "Variation_Forest",
    primary.column = "Deforestation",
    show.correlation = FALSE
  )

  expect_match(with_correlation$labels$subtitle, "Maior correlação", fixed = TRUE)
  expect_null(without_correlation$labels$subtitle)
  expect_identical(without_correlation$labels$title, with_correlation$labels$title)
})

test_that("result grouping discovery includes levels and numeric identifiers", {
  expect_identical(
    canonical_result_level(c("grid", "Malha", "Grupos", "Grupos 1", "Grupos 2", "Total")),
    c("grid", "mesh", "groups", "groups_1", "groups_2", "total")
  )
  choices <- chart_grouping_choices(
    c("grid", "mesh", "groups", "groups_1", "groups_2", "total"),
    c("Grid_ID", "group_code")
  )
  expect_setequal(
    unname(choices),
    c(
      "__none__", "__level__:grid", "__level__:mesh", "__level__:groups",
      "__level__:groups_1", "__level__:groups_2", "__level__:total",
      "__column__:Grid_ID", "__column__:group_code"
    )
  )

  grid <- data.frame(
    Grid_ID = c(1L, 2L),
    Year = c(2020L, 2020L),
    Forest = c(10, 20),
    Variation_Forest = c(NA, NA),
    Deforestation = c(1, 2)
  )
  expect_identical(result_group_columns(grid), "Grid_ID")

  groups <- data.frame(
    state = c("AM", "PA"),
    group_code = c(10L, 20L),
    Year = c(2020L, 2020L),
    Forest = c(10, 20),
    Variation_Forest = c(NA, NA),
    Deforestation = c(1, 2)
  )
  expect_setequal(result_group_columns(groups, "groups"), c("state", "group_code"))
  prepared <- prepare_chart_level_data(groups, "groups")
  expect_identical(prepared$group, ".LandScriptChartGroup")
  expect_match(prepared$data$.LandScriptChartGroup[[1]], "state: AM", fixed = TRUE)
  expect_match(prepared$data$.LandScriptChartGroup[[1]], "group_code: 10", fixed = TRUE)
})

test_that("download ZIP is assembled directly from final Excel and GeoPackage", {
  folder <- tempfile("result_zip_")
  dir.create(folder)
  excel <- file.path(folder, "analysis.xlsx")
  gpkg <- file.path(folder, "analysis.gpkg")
  summary <- file.path(folder, "analysis_ResumoExecucao.txt")
  writeLines("excel", excel)
  writeLines("gpkg", gpkg)
  writeLines("summary", summary)
  result <- list(files = c(
    complete_xlsx = excel,
    complete_gpkg = gpkg,
    run_summary = summary
  ))
  destination <- tempfile("download_", fileext = ".zip")

  prepare_result_download_zip(result, destination)
  entries <- utils::unzip(destination, list = TRUE)$Name
  expect_setequal(entries, c("analysis.xlsx", "analysis.gpkg", "analysis_ResumoExecucao.txt"))
})

test_that("raster validation detects exact grid alignment", {
  folder <- tempfile("aligned_grids_")
  dir.create(folder)

  aligned_1 <- terra::rast(
    nrows = 10, ncols = 10, xmin = 0, xmax = 100,
    ymin = 0, ymax = 100, crs = "EPSG:3857"
  )
  aligned_2 <- aligned_1
  shifted <- terra::rast(
    nrows = 10, ncols = 10, xmin = 5, xmax = 105,
    ymin = 0, ymax = 100, crs = "EPSG:3857"
  )
  terra::values(aligned_1) <- 1
  terra::values(aligned_2) <- 2
  terra::values(shifted) <- 3
  terra::writeRaster(aligned_1, file.path(folder, "classes_2020.tif"), overwrite = TRUE)
  terra::writeRaster(aligned_2, file.path(folder, "classes_2021.tif"), overwrite = TRUE)

  aligned_inspection <- inspect_raster_folder(folder)
  expect_true(aligned_inspection$grids_aligned)
  expect_equal(aligned_inspection$grid_group_count, 1L)
  expect_equal(length(unique(aligned_inspection$index$grid_signature)), 1L)

  terra::writeRaster(shifted, file.path(folder, "classes_2022.tif"), overwrite = TRUE)
  mixed_inspection <- inspect_raster_folder(folder)
  expect_false(mixed_inspection$grids_aligned)
  expect_equal(mixed_inspection$grid_group_count, 2L)
})

test_that("zonal extraction crops to the study area and reuses aligned zones", {
  folder <- tempfile("zonal_crop_")
  dir.create(folder)
  raster_1 <- terra::rast(
    nrows = 100, ncols = 100, xmin = 0, xmax = 100,
    ymin = 0, ymax = 100, crs = "EPSG:3857"
  )
  raster_2 <- raster_1
  terra::values(raster_1) <- rep(c(1, 2), length.out = terra::ncell(raster_1))
  terra::values(raster_2) <- rep(c(2, 3), length.out = terra::ncell(raster_2))
  path_1 <- file.path(folder, "classes_2020.tif")
  path_2 <- file.path(folder, "classes_2021.tif")
  terra::writeRaster(raster_1, path_1, overwrite = TRUE)
  terra::writeRaster(raster_2, path_2, overwrite = TRUE)

  mesh <- sf::st_sf(
    ID_mesh = "M1",
    geometry = sf::st_as_sfc(
      sf::st_bbox(c(xmin = 20, ymin = 20, xmax = 40, ymax = 40), crs = sf::st_crs(3857))
    )
  )

  old_full_extent_extract <- function(path, year) {
    raster <- terra::rast(path)
    zones <- terra::rasterize(terra::vect(mesh), raster, field = "ID_mesh", background = NA)
    names(zones) <- "ID_mesh"
    cross <- terra::crosstab(c(zones, raster), long = TRUE, useNA = FALSE)
    names(cross)[1:3] <- c("ID_mesh", "Class", "Pixels")
    cross$Class <- as.character(cross$Class)
    wide <- tidyr::pivot_wider(
      as.data.frame(cross), id_cols = "ID_mesh", names_from = "Class",
      values_from = "Pixels", values_fill = 0, names_repair = "minimal"
    )
    wide$Year <- as.integer(year)
    wide
  }

  expected <- old_full_extent_extract(path_1, 2020)
  cache <- new_zonal_cache()
  actual <- extract_zonal_counts(path_1, mesh, 2020, cache, "grid_1")
  extract_zonal_counts(path_2, mesh, 2021, cache, "grid_1")

  expect_equal(actual, expected, ignore_attr = TRUE)
  expect_equal(cache$rasterizations, 1L)
  expect_equal(cache$zone_cache_hits, 1L)
  expect_lt(cache$processed_cells, cache$source_cells)
})
