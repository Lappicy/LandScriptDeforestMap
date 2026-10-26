column_name_fixture <- function() {
  data <- data.frame(
    Year = rep(2020:2023, 2),
    Grid_ID = rep(1:2, each = 4),
    check.names = FALSE
  )
  classes <- c(
    "Bare Soil", "Sandy Formations", "Rock & sand $", "Bare.Soil",
    "Soil_type", "Água / solo (50%)"
  )
  for (i in seq_along(classes)) {
    data[[classes[[i]]]] <- (seq_len(nrow(data)) + i)^2
    data[[paste0("Variation_", classes[[i]])]] <- seq_len(nrow(data)) * i
  }
  data[["Growth_Rock & sand $"]] <- seq_len(nrow(data)) * 2
  data[["Region & budget $"]] <- rep(c("North", "South"), each = 4)
  data
}

test_that("result imports preserve spaces and symbols alongside valid geometry", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("sf")
  folder <- tempfile("column_names_")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  data <- column_name_fixture()

  xlsx <- file.path(folder, "result.xlsx")
  writexl::write_xlsx(list(Grid_ID = data), xlsx)
  imported_excel <- read_result_dataset(xlsx)
  expect_identical(names(imported_excel), names(data))
  expect_equal(imported_excel, data, ignore_attr = TRUE)

  polygon <- sf::st_polygon(list(rbind(
    c(-48, -16), c(-47.99, -16), c(-47.99, -15.99),
    c(-48, -15.99), c(-48, -16)
  )))
  for (geometry_column in c("geom", "geometry")) {
    spatial <- data
    spatial[[geometry_column]] <- sf::st_sfc(
      rep(list(polygon), nrow(data)), crs = 4326
    )
    spatial <- sf::st_as_sf(spatial, sf_column_name = geometry_column)
    gpkg <- file.path(folder, paste0(geometry_column, ".gpkg"))
    sf::st_write(spatial, gpkg, layer = "grid", quiet = TRUE,
                 layer_options = paste0("GEOMETRY_NAME=", geometry_column))
    imported <- read_result_dataset(gpkg)
    expect_s3_class(imported, "sf")
    expect_identical(attr(imported, "sf_column"), geometry_column)
    expect_identical(names(result_table_data(imported)), names(data))
    expect_equal(result_table_data(imported), data, ignore_attr = TRUE)
    expect_identical(sf::st_as_binary(sf::st_geometry(imported)),
                     sf::st_as_binary(sf::st_geometry(spatial)))
    expect_true(sf::st_crs(imported) == sf::st_crs(spatial))

    chart <- build_timeseries_plot(
      result_table_data(imported),
      comparison.columns = "Variation_Rock & sand $",
      primary.column = "Bare Soil", language = "en"
    )
    expect_identical(chart$scales$get_scales("colour")$labels,
                     c("Bare Soil", "Rock & sand $ variation"))
    expect_silent(ggplot2::ggplot_build(chart))
    map <- mesh.map(imported, class = "Variation_Rock & sand $", language = "en")
    expect_identical(map$labels$fill, "Rock & sand $ variation (km²)")
    expect_match(map$labels$title, "Rock & sand $ variation", fixed = TRUE)
    expect_identical(map$scales$get_scales("fill")$breaks[[1]],
                     "No Rock & sand $ variation")
    expect_silent(ggplot2::ggplot_build(map))
  }
})

test_that("plot labels and facets keep custom characters through export", {
  data <- column_name_fixture()
  classes <- c("Bare Soil", "Sandy Formations", "Rock & sand $", "Bare.Soil",
               "Soil_type", "Água / solo (50%)")
  expect_identical(plot_class_label(classes, "en"), classes)
  expect_identical(plot_class_label(classes, "pt-BR"), classes)
  expect_identical(plot_class_label(paste0("Variation_", classes), "en"),
                   paste0(classes, " variation"))
  expect_identical(plot_class_label(paste0("Variation_", classes), "pt-BR"),
                   paste0("Variação de ", classes))
  expect_identical(plot_class_label("Growth_Rock & sand $", "en"),
                   "Rock & sand $ growth")
  expect_identical(plot_class_label(c("Forest", "Variation_Forest"), "pt-BR"),
                   c("Floresta", "Variação de Floresta"))

  comparisons <- c("Sandy Formations", "Variation_Bare Soil", "Variation_Sandy Formations",
                   "Rock & sand $", "Variation_Rock & sand $", "Bare.Soil", "Soil_type")
  plot <- build_timeseries_plot(
    data, comparison.columns = comparisons, primary.column = "Bare Soil",
    different.group = "Region & budget $", language = "en"
  )
  expect_identical(plot$scales$get_scales("colour")$labels,
                   plot_class_label(c("Bare Soil", comparisons), "en"))
  expect_match(plot$labels$subtitle, "Sandy Formations", fixed = TRUE)
  variation_plot <- build_timeseries_plot(
    data, comparison.columns = "Variation_Rock & sand $",
    primary.column = "Bare Soil", language = "en"
  )
  expect_match(variation_plot$labels$subtitle, "Rock & sand $ variation", fixed = TRUE)
  expect_silent(built <- ggplot2::ggplot_build(plot))
  expect_equal(nrow(built$layout$layout), 2L)
  expect_setequal(as.character(plot$data$Classe), c("Bare Soil", comparisons))

  folder <- tempfile("plot_names_")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  for (extension in c("png", "pdf")) {
    output <- file.path(folder, paste0("chart.", extension))
    ggplot2::ggsave(output, plot, width = 10, height = 5)
    expect_gt(file.info(output)$size, 0)
  }
})
