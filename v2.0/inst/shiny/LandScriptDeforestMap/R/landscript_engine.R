mapbiomas_class_map <- function(version = NULL, custom = NULL) {
  version <- as.character(version %||% "")
  maps <- list(
    `4` = list(
      Forest = c(1, 2, 3),
      NonForest = c(4, 5, 9, 10, 11, 12, 13, 14, 15, 18, 19, 20, 21, 22, 23, 24, 25, 29, 30, 32),
      Water = c(26, 31, 33), Others = c(0, 27), Urban = 24, Mining = 30,
      Pasture = 15, Agriculture = 18
    ),
    `7.1` = list(
      Forest = c(1, 3),
      NonForest = c(4, 5, 49, 10, 11, 12, 32, 29, 50, 13, 14, 15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 48, 9, 21, 22, 23, 24, 30, 25),
      Water = c(26, 31, 33), Others = c(0, 27), Urban = 24, Mining = 30,
      Pasture = 15, Agriculture = c(9, 18, 19, 20, 36, 39, 40, 41, 46, 47, 48, 62)
    ),
    `8` = list(
      Forest = c(1, 3),
      NonForest = c(4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13, 14, 15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21, 22, 23, 24, 30, 25),
      Water = c(26, 31, 33), Others = c(0, 27), Urban = 24, Mining = 30,
      Pasture = 15, Agriculture = c(9, 18, 19, 20, 35, 36, 39, 40, 41, 46, 47, 48, 62)
    ),
    `10` = list(
      Forest = c(1, 3),
      NonForest = c(4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 14, 15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21, 22, 23, 24, 30, 75, 25),
      Water = c(26, 31, 33), Others = c(0, 27), Urban = 24, Mining = 30,
      CropLivestock = 14, Pasture = 15,
      Agriculture = c(9, 18, 19, 20, 21, 35, 36, 39, 40, 41, 46, 47, 48, 62)
    )
  )

  if (identical(version, "custom")) {
    if (is.null(custom) || !length(custom)) {
      stop("Defina as classes personalizadas.", call. = FALSE)
    }
    return(custom[lengths(custom) > 0L])
  }
  if (!version %in% names(maps)) {
    stop("Versão MAPBIOMAS não suportada: ", version, call. = FALSE)
  }
  maps[[version]]
}

count.classes <- function(proxy.table, MAPBIOMAS = NULL, custom.classes = NULL) {
  mapping <- mapbiomas_class_map(MAPBIOMAS, custom.classes)
  numeric_values <- suppressWarnings(as.numeric(names(proxy.table)))
  numeric_names <- !is.na(numeric_values)

  for (class_name in names(mapping)) {
    columns <- which(numeric_names & numeric_values %in% mapping[[class_name]])
    proxy.table[[class_name]] <- if (length(columns)) {
      rowSums(as.data.frame(proxy.table[, columns, drop = FALSE]), na.rm = TRUE)
    } else {
      0
    }
  }
  attr(proxy.table, "summary_class_columns") <- names(mapping)
  proxy.table
}

add_variations <- function(data, id_columns, value_columns, year_column = "Year") {
  if (!length(value_columns)) return(data)
  data[[year_column]] <- as.integer(data[[year_column]])
  ordering <- if (length(id_columns)) {
    do.call(order, c(data[id_columns], list(data[[year_column]])))
  } else {
    order(data[[year_column]])
  }
  data <- data[ordering, , drop = FALSE]

  key <- if (length(id_columns)) {
    interaction(data[id_columns], drop = TRUE, lex.order = TRUE)
  } else {
    factor(rep("all", nrow(data)))
  }
  groups <- split(seq_len(nrow(data)), key)

  for (column in value_columns) {
    variation <- rep(NA_real_, nrow(data))
    values <- as.numeric(data[[column]])
    values[is.na(values)] <- 0
    for (rows in groups) {
      ordered_rows <- rows[order(data[[year_column]][rows])]
      variation[ordered_rows] <- c(NA_real_, diff(values[ordered_rows]))
    }
    data[[paste0("Variation_", column)]] <- variation
  }

  if ("Variation_Forest" %in% names(data)) {
    data$Deforestation <- pmax(-data$Variation_Forest, 0)
    data$Reforestation <- pmax(data$Variation_Forest, 0)
  }
  rownames(data) <- NULL
  data
}

extract_zonal_counts <- function(raster_path, mesh, year) {
  raster <- terra::rast(raster_path)
  if (terra::nlyr(raster) != 1L) {
    stop("O raster deve possuir uma única camada: ", basename(raster_path), call. = FALSE)
  }
  if (!nzchar(terra::crs(raster))) {
    stop("Raster sem CRS: ", basename(raster_path), call. = FALSE)
  }

  mesh_raster_crs <- sf::st_transform(mesh, terra::crs(raster))
  zones <- terra::rasterize(
    terra::vect(mesh_raster_crs),
    raster,
    field = "ID_mesh",
    background = NA
  )
  names(zones) <- "ID_mesh"

  cross <- terra::crosstab(c(zones, raster), long = TRUE, useNA = FALSE)
  base <- data.frame(ID_mesh = mesh$ID_mesh, stringsAsFactors = FALSE)

  if (is.null(cross) || !nrow(cross)) {
    base$Year <- as.integer(year)
    return(base)
  }

  names(cross)[1:3] <- c("ID_mesh", "Class", "Pixels")
  cross$Class <- as.character(cross$Class)
  wide <- tidyr::pivot_wider(
    as.data.frame(cross),
    id_cols = "ID_mesh",
    names_from = "Class",
    values_from = "Pixels",
    values_fill = 0,
    names_repair = "minimal"
  )
  out <- dplyr::left_join(base, wide, by = "ID_mesh")
  class_columns <- setdiff(names(out), "ID_mesh")
  out[class_columns][is.na(out[class_columns])] <- 0
  out$Year <- as.integer(year)
  out
}

aggregate_result_level <- function(
  unit_table,
  geometry,
  by_columns,
  value_columns,
  level_name,
  group_column
) {
  summary <- unit_table |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by_columns, "Year")))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_columns), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  summary <- add_variations(
    summary,
    id_columns = by_columns,
    value_columns = value_columns
  )

  if (identical(level_name, "Grupo")) {
    geo <- geometry[, c(group_column, "geometry")]
    summary <- dplyr::left_join(summary, geo, by = group_column)
  } else {
    geo <- geometry
    summary$BoundaryGroup <- "Área total"
    summary$geometry <- rep(sf::st_geometry(geo), nrow(summary))
  }

  summary$AnalysisLevel <- level_name
  sf::st_as_sf(summary, sf_column_name = "geometry", crs = sf::st_crs(geometry))
}

drop_raw_class_columns <- function(data) {
  geometry_column <- attr(data, "sf_column") %||% "geometry"
  names_to_drop <- names(data)[
    !is.na(suppressWarnings(as.numeric(names(data)))) |
      grepl("^Variation_[0-9]+$", names(data))
  ]
  data[, setdiff(names(data), names_to_drop), drop = FALSE]
}

write_result_layers <- function(path, layers, simplified = FALSE) {
  if (file.exists(path)) unlink(path)
  first <- TRUE
  for (layer_name in names(layers)) {
    object <- layers[[layer_name]]
    if (simplified) object <- drop_raw_class_columns(object)
    sf::st_write(
      object,
      dsn = path,
      layer = layer_name,
      append = !first,
      quiet = TRUE
    )
    first <- FALSE
  }
  normalizePath(path, mustWork = TRUE)
}

run_land_analysis <- function(params, progress_file = NULL) {
  progress <- function(percent, stage, detail = NULL, status = "running") {
    if (!is.null(progress_file)) {
      write_progress(progress_file, percent, stage, detail, status)
    }
  }

  progress(2, "Validação", "Conferindo arquivos e parâmetros")
  raster_index <- list_raster_files(params$raster_folder)
  output_folder <- ensure_output_folder(params$output_folder)
  output_name <- sanitize_output_name(params$output_name)

  progress(6, "Leitura", "Abrindo e validando o arquivo geoespacial")
  geo <- read_geo(params$geo_path)
  group_column <- params$group_column
  if (is.null(group_column) || !nzchar(group_column) || identical(group_column, ".ALL")) {
    group_column <- "BoundaryGroup"
  }

  progress(10, "Limites", "Dissolvendo classes e removendo sobreposições")
  boundaries <- prepare_group_boundaries(geo, params$group_column)
  group_column <- attr(boundaries, "group_column")
  overlap_removed <- isTRUE(attr(boundaries, "overlap_removed"))

  progress(14, "Malha", "Criando e recortando a malha quadrangular")
  mesh <- create.mesh(
    boundaries,
    mesh.size = if (isTRUE(params$no_mesh)) NULL else params$mesh_size,
    group.column = group_column,
    max.cells = params$max_cells %||% 50000L,
    mesh.unit = params$mesh_unit %||% "degrees"
  )
  mesh[[group_column]] <- factor(mesh[[group_column]])

  all_counts <- vector("list", nrow(raster_index))
  for (i in seq_len(nrow(raster_index))) {
    pct <- 15 + 55 * (i - 1) / max(1, nrow(raster_index))
    progress(
      pct,
      paste0("Raster ", i, " de ", nrow(raster_index)),
      paste0(raster_index$year[[i]], " — ", raster_index$file[[i]])
    )
    all_counts[[i]] <- extract_zonal_counts(
      raster_index$path[[i]],
      mesh,
      raster_index$year[[i]]
    )
  }

  progress(71, "Conversão", "Combinando anos e convertendo pixels para km²")
  counts <- dplyr::bind_rows(all_counts)
  raw_class_columns <- names(counts)[!is.na(suppressWarnings(as.numeric(names(counts))))]
  counts[raw_class_columns] <- lapply(
    counts[raw_class_columns],
    function(x) as.numeric(x) * params$pixel_km2_ratio
  )

  attributes <- sf::st_drop_geometry(mesh)
  unit_table <- dplyr::left_join(counts, attributes, by = "ID_mesh")
  unit_table[[group_column]] <- factor(unit_table[[group_column]])

  progress(78, "Classes", paste0("Aplicando a legenda MAPBIOMAS ", params$mapbiomas))
  unit_table <- count.classes(
    unit_table,
    MAPBIOMAS = params$mapbiomas,
    custom.classes = params$custom_classes
  )
  summary_class_columns <- attr(unit_table, "summary_class_columns")
  value_columns <- c(raw_class_columns, summary_class_columns)

  progress(84, "Variações", "Calculando variações anuais por unidade espacial")
  unit_table <- add_variations(
    unit_table,
    id_columns = "ID_mesh",
    value_columns = value_columns
  )
  unit_table$AnalysisLevel <- "Malha"
  unit_results <- dplyr::left_join(unit_table, mesh[, c("ID_mesh", "geometry")], by = "ID_mesh")
  unit_results <- sf::st_as_sf(unit_results, sf_column_name = "geometry", crs = sf::st_crs(mesh))

  progress(89, "Agregação", "Gerando resultados por grupo e para a área total")
  group_geometry <- boundaries[, c(group_column, "geometry")]
  group_results <- aggregate_result_level(
    unit_table,
    group_geometry,
    by_columns = group_column,
    value_columns = value_columns,
    level_name = "Grupo",
    group_column = group_column
  )

  total_geometry <- sf::st_sf(
    geometry = sf::st_sfc(suppressWarnings(sf::st_union(boundaries)), crs = sf::st_crs(boundaries))
  )
  total_results <- aggregate_result_level(
    unit_table,
    total_geometry,
    by_columns = character(),
    value_columns = value_columns,
    level_name = "Total",
    group_column = group_column
  )

  progress(94, "Exportação", "Gravando tabelas e GeoPackages")
  layers <- list(mesh = unit_results, groups = group_results, total = total_results)
  complete_table <- dplyr::bind_rows(lapply(layers, sf::st_drop_geometry))
  simplified_table <- dplyr::bind_rows(lapply(layers, function(x) {
    sf::st_drop_geometry(drop_raw_class_columns(x))
  }))

  complete_txt <- file.path(output_folder, paste0(output_name, ".txt"))
  simplified_txt <- file.path(output_folder, paste0("Simplified_", output_name, ".txt"))
  complete_gpkg <- file.path(output_folder, paste0(output_name, ".gpkg"))
  simplified_gpkg <- file.path(output_folder, paste0("Simplified_", output_name, ".gpkg"))
  complete_xlsx <- file.path(output_folder, paste0(output_name, ".xlsx"))
  simplified_xlsx <- file.path(output_folder, paste0("Simplified_", output_name, ".xlsx"))
  pixels_txt <- file.path(output_folder, paste0(output_name, "_CalcRasterPixels.txt"))
  result_rds <- file.path(output_folder, paste0(output_name, ".rds"))

  utils::write.table(
    complete_table, complete_txt, sep = "\t", dec = ".", quote = FALSE,
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  utils::write.table(
    simplified_table, simplified_txt, sep = "\t", dec = ".", quote = FALSE,
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  utils::write.table(
    counts, pixels_txt, sep = "\t", dec = ".", quote = FALSE,
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  write_result_layers(complete_gpkg, layers, simplified = FALSE)
  write_result_layers(simplified_gpkg, layers, simplified = TRUE)
  writexl::write_xlsx(
    lapply(layers, sf::st_drop_geometry),
    complete_xlsx
  )
  writexl::write_xlsx(
    lapply(layers, function(x) sf::st_drop_geometry(drop_raw_class_columns(x))),
    simplified_xlsx
  )

  result <- list(
    mesh = unit_results,
    groups = group_results,
    total = total_results,
    complete_table = complete_table,
    simplified_table = simplified_table,
    raster_index = raster_index,
    group_column = group_column,
    summary_class_columns = summary_class_columns,
    output_name = output_name,
    output_folder = output_folder,
    overlap_removed = overlap_removed,
    files = c(
      complete_table = normalizePath(complete_txt),
      simplified_table = normalizePath(simplified_txt),
      complete_gpkg = normalizePath(complete_gpkg),
      simplified_gpkg = normalizePath(simplified_gpkg),
      complete_xlsx = normalizePath(complete_xlsx),
      simplified_xlsx = normalizePath(simplified_xlsx),
      pixel_counts = normalizePath(pixels_txt),
      result_rds = normalizePath(result_rds, mustWork = FALSE)
    )
  )
  saveRDS(result, result_rds)

  progress(100, "Concluído", "Análise finalizada e arquivos exportados", "complete")
  result
}

run_analysis_job <- function(params, progress_file, result_file, app_directory) {
  options(landscript.app_dir = app_directory)
  source(file.path(app_directory, "R", "utils.R"), local = globalenv())
  source(file.path(app_directory, "R", "spatial_io.R"), local = globalenv())
  source(file.path(app_directory, "R", "landscript_engine.R"), local = globalenv())

  tryCatch({
    result <- run_land_analysis(params, progress_file)
    saveRDS(result, result_file)
    invisible(result_file)
  }, error = function(e) {
    write_progress(progress_file, 100, "Erro", conditionMessage(e), "error")
    stop(e)
  })
}
