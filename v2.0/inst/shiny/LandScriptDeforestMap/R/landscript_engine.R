mapbiomas_class_map <- function(version = NULL, custom = NULL) {
  version <- as.character(version %||% "")
  if (identical(version, "none")) {
    return(list())
  }
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
  if (!length(mapping)) {
    attr(proxy.table, "summary_class_columns") <- character()
    return(proxy.table)
  }
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

new_zonal_cache <- function() {
  cache <- new.env(parent = emptyenv())
  cache$zones <- list()
  cache$mesh_entries <- list()
  cache$zone_cache_hits <- 0L
  cache$rasterizations <- 0L
  cache$source_cells <- 0
  cache$processed_cells <- 0
  cache
}

cached_mesh_vector <- function(mesh, raster, cache = NULL) {
  raster_crs <- terra::crs(raster)
  if (!is.null(cache)) {
    entries <- cache$mesh_entries
    if (length(entries)) {
      matches <- vapply(entries, function(entry) identical(entry$crs, raster_crs), logical(1))
      if (any(matches)) return(entries[[which(matches)[[1]]]]$mesh)
    }
  }

  transformed <- terra::vect(sf::st_transform(mesh, raster_crs))
  if (!is.null(cache)) {
    cache$mesh_entries <- c(
      cache$mesh_entries,
      list(list(crs = raster_crs, mesh = transformed))
    )
  }
  transformed
}

crop_raster_to_mesh <- function(raster, mesh_vector, raster_path = NULL) {
  cropped <- tryCatch(
    terra::crop(raster, terra::ext(mesh_vector), snap = "out"),
    error = function(e) {
      stop(
        "O raster ", basename(raster_path %||% "selecionado"),
        " não intersecta a área de estudo: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!terra::ncell(cropped) || terra::nrow(cropped) < 1L || terra::ncol(cropped) < 1L) {
    stop(
      "O raster ", basename(raster_path %||% "selecionado"),
      " não possui células na extensão da área de estudo.",
      call. = FALSE
    )
  }
  cropped
}

extract_zonal_counts <- function(
  raster_path,
  mesh,
  year,
  zone_cache = NULL,
  cache_key = NULL
) {
  raster <- terra::rast(raster_path)
  if (terra::nlyr(raster) != 1L) {
    stop("O raster deve possuir uma única camada: ", basename(raster_path), call. = FALSE)
  }
  if (!nzchar(terra::crs(raster))) {
    stop("Raster sem CRS: ", basename(raster_path), call. = FALSE)
  }

  mesh_vector <- cached_mesh_vector(mesh, raster, zone_cache)
  raster_aoi <- crop_raster_to_mesh(raster, mesh_vector, raster_path)

  zones <- NULL
  if (!is.null(zone_cache) && !is.null(cache_key) && nzchar(cache_key)) {
    cached <- zone_cache$zones[[cache_key]]
    geometry_matches <- !is.null(cached) && isTRUE(tryCatch(
      terra::compareGeom(cached, raster_aoi, stopOnError = FALSE),
      error = function(e) FALSE
    ))
    if (geometry_matches) {
      zones <- cached
      zone_cache$zone_cache_hits <- zone_cache$zone_cache_hits + 1L
    }
  }

  if (is.null(zones)) {
    zones <- terra::rasterize(
      mesh_vector,
      raster_aoi,
      field = "ID_mesh",
      background = NA
    )
    if (!is.null(zone_cache)) {
      zone_cache$rasterizations <- zone_cache$rasterizations + 1L
      if (!is.null(cache_key) && nzchar(cache_key)) {
        zone_cache$zones[[cache_key]] <- zones
      }
    }
  }
  names(zones) <- "ID_mesh"

  if (!is.null(zone_cache)) {
    zone_cache$source_cells <- zone_cache$source_cells + terra::ncell(raster)
    zone_cache$processed_cells <- zone_cache$processed_cells + terra::ncell(raster_aoi)
  }

  cross <- terra::crosstab(c(zones, raster_aoi), long = TRUE, useNA = FALSE)
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
  level_name
) {
  by_columns <- normalize_group_columns(by_columns, max_columns = 2L)
  if (length(by_columns)) {
    summary <- unit_table |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(by_columns, "Year")))) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(value_columns), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  } else {
    summary <- unit_table |>
      dplyr::group_by(.data$Year) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(value_columns), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  summary <- add_variations(
    summary,
    id_columns = by_columns,
    value_columns = value_columns
  )

  if (length(by_columns)) {
    geo <- geometry[, c(by_columns, "geometry")]
    summary <- dplyr::left_join(summary, geo, by = by_columns)
  } else {
    geo <- geometry
    summary$BoundaryGroup <- "Área total"
    summary$geometry <- rep(sf::st_geometry(geo), nrow(summary))
  }

  summary$AnalysisLevel <- level_name
  sf::st_as_sf(summary, sf_column_name = "geometry", crs = sf::st_crs(geometry))
}

aggregate_boundary_geometry <- function(boundaries, by_columns) {
  by_columns <- normalize_group_columns(by_columns, max_columns = 2L)
  if (!length(by_columns)) return(boundaries)

  previous_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(previous_s2)), add = TRUE)

  out <- boundaries |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by_columns))) |>
    dplyr::summarise(geometry = suppressMessages(suppressWarnings(sf::st_union(.data$geometry))), .groups = "drop")
  sf::st_as_sf(out, sf_column_name = "geometry", crs = sf::st_crs(boundaries))
}

aggregate_grid_result <- function(unit_table, mesh, value_columns) {
  if (!"Grid_ID" %in% names(unit_table) || !"Grid_ID" %in% names(mesh)) {
    stop("A tabela da malha precisa conter Grid_ID para agregar por quadrícula.", call. = FALSE)
  }

  summary <- unit_table |>
    dplyr::group_by(.data$Grid_ID, .data$Year) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_columns), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  summary <- add_variations(
    summary,
    id_columns = "Grid_ID",
    value_columns = value_columns
  )

  previous_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(previous_s2)), add = TRUE)

  mesh_geometry <- mesh[, c("Grid_ID", "geometry")]
  grid_geometry <- tryCatch({
    metric <- sf::st_transform(mesh_geometry, 5880)
    metric <- metric |>
      dplyr::group_by(.data$Grid_ID) |>
      dplyr::summarise(geometry = suppressMessages(suppressWarnings(sf::st_union(.data$geometry))), .groups = "drop")
    sf::st_transform(metric, sf::st_crs(mesh))
  }, error = function(e) {
    mesh_geometry |>
      dplyr::group_by(.data$Grid_ID) |>
      dplyr::summarise(geometry = suppressMessages(suppressWarnings(sf::st_union(.data$geometry))), .groups = "drop")
  })

  summary <- dplyr::left_join(summary, grid_geometry, by = "Grid_ID")
  summary$AnalysisLevel <- "Quadrícula"
  sf::st_as_sf(summary, sf_column_name = "geometry", crs = sf::st_crs(mesh))
}

write_result_layers <- function(path, layers) {
  if (file.exists(path)) unlink(path)
  first <- TRUE
  for (layer_name in names(layers)) {
    object <- layers[[layer_name]]
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

split_result_layers <- function(layers) {
  group_layer_names <- grep("^groups", names(layers), value = TRUE)
  list(
    ID_mesh = layers["mesh"],
    Grid_ID = layers["grid"],
    Grupos = layers[group_layer_names],
    Total = layers["total"]
  )
}

write_result_geopackages <- function(
  output_folder,
  output_name,
  layers,
  threshold_bytes = 2 * 1024^3
) {
  # Most analyses fit in one GeoPackage. Write that common case once and only
  # split it if the actual file exceeds the configured threshold. The previous
  # flow wrote every split file first and then wrote all layers a second time.
  combined_path <- file.path(output_folder, paste0(output_name, ".gpkg"))
  write_result_layers(combined_path, layers)
  combined_size <- file.info(combined_path)$size
  if (is.finite(combined_size) && combined_size <= threshold_bytes) {
    return(setNames(normalizePath(combined_path, mustWork = TRUE), "complete_gpkg"))
  }

  safe_unlink(combined_path)
  split_layers <- split_result_layers(layers)
  split_layers <- split_layers[lengths(split_layers) > 0L]
  split_paths <- vapply(names(split_layers), function(layer_group) {
    path <- file.path(output_folder, paste0(output_name, "_", layer_group, ".gpkg"))
    write_result_layers(path, split_layers[[layer_group]])
  }, character(1))

  stats::setNames(
    normalizePath(split_paths, mustWork = TRUE),
    paste0("complete_gpkg_", tolower(names(split_paths)))
  )
}

result_excel_sheets <- function(layers) {
  sheet_names <- c(
    grid = "Grid_ID",
    mesh = "Malha",
    groups = "Grupos",
    groups_1 = "Grupos 1",
    groups_2 = "Grupos 2",
    total = "Total"
  )
  sheets <- lapply(layers, function(object) {
    sf::st_drop_geometry(object)
  })
  names(sheets) <- unname(sheet_names[names(sheets)] %||% names(sheets))
  sheets
}

write_result_workbook <- function(path, layers) {
  if (file.exists(path)) unlink(path)
  writexl::write_xlsx(
    result_excel_sheets(layers),
    path
  )
  normalizePath(path, mustWork = TRUE)
}

analysis_proxy_dir <- function(output_folder, output_name) {
  file.path(output_folder, paste0(output_name, "_proxy"))
}

proxy_checkpoint_path <- function(proxy_dir, name) {
  file.path(proxy_dir, paste0(name, ".rds"))
}

write_checkpoint <- function(object, proxy_dir, name) {
  dir.create(proxy_dir, recursive = TRUE, showWarnings = FALSE)
  path <- proxy_checkpoint_path(proxy_dir, name)
  tmp <- tempfile(paste0(name, "_"), tmpdir = proxy_dir, fileext = ".rds")
  saveRDS(object, tmp)
  if (file.exists(path)) unlink(path)
  file.rename(tmp, path)
  normalizePath(path, mustWork = TRUE)
}

read_checkpoint <- function(proxy_dir, name) {
  path <- proxy_checkpoint_path(proxy_dir, name)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

copy_files_to_proxy <- function(files, destination) {
  files <- normalizePath(files[file.exists(files)], mustWork = TRUE)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  target_names <- basename(files)
  if (anyDuplicated(target_names)) {
    target_names <- make.unique(target_names, sep = "_")
  }
  targets <- file.path(destination, target_names)
  ok <- file.copy(files, targets, overwrite = TRUE, copy.mode = FALSE)
  if (!all(ok)) {
    stop(
      "Não foi possível copiar os arquivos para a pasta proxy: ",
      paste(basename(files[!ok]), collapse = ", "),
      call. = FALSE
    )
  }
  normalizePath(targets, mustWork = TRUE)
}

geo_bundle_files <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  ext <- tolower(tools::file_ext(path))
  if (!identical(ext, "shp")) return(path)
  base <- tolower(tools::file_path_sans_ext(basename(path)))
  siblings <- list.files(dirname(path), full.names = TRUE, recursive = FALSE)
  siblings[
    tolower(tools::file_path_sans_ext(basename(siblings))) == base &
      tolower(tools::file_ext(siblings)) %in% shapefile_extensions()
  ]
}

raster_index_for_params <- function(params) {
  index <- params$raster_index
  required <- c("path", "file", "year")
  valid_cached_index <- is.data.frame(index) &&
    nrow(index) > 0L &&
    all(required %in% names(index)) &&
    all(file.exists(index$path))

  if (!isTRUE(valid_cached_index)) {
    return(ensure_raster_grid_signatures(list_raster_files(params$raster_folder)))
  }

  optional <- intersect(c("pixel_area_km2", "grid_signature", "grid_group"), names(index))
  index <- index[, c(required, optional), drop = FALSE]
  index$path <- normalizePath(index$path, mustWork = TRUE)
  index$file <- basename(index$path)
  index$year <- as.integer(index$year)
  if (anyNA(index$year) || anyDuplicated(index$year)) {
    return(ensure_raster_grid_signatures(list_raster_files(params$raster_folder)))
  }
  index <- index[order(index$year), , drop = FALSE]
  ensure_raster_grid_signatures(index)
}

analysis_fingerprint <- function(params) {
  geo_files <- geo_bundle_files(params$geo_path)
  geo_info <- file.info(geo_files)
  raster_index <- ensure_raster_grid_signatures(raster_index_for_params(params))
  raster_info <- file.info(raster_index$path)
  list(
    geo = data.frame(
      file = basename(geo_files),
      size = unname(geo_info$size),
      stringsAsFactors = FALSE
    ),
    rasters = data.frame(
      file = raster_index$file,
      year = raster_index$year,
      size = unname(raster_info$size),
      stringsAsFactors = FALSE
    ),
    group_column = normalize_group_columns(params$group_column %||% character(), max_columns = 2L),
    no_mesh = isTRUE(params$no_mesh),
    mesh_size = params$mesh_size %||% NA_real_,
    mesh_unit = params$mesh_unit %||% "degrees",
    mapbiomas = params$mapbiomas %||% "",
    custom_classes = params$custom_classes,
    pixel_km2_ratio = params$pixel_km2_ratio,
    max_cells = params$max_cells %||% NA_integer_,
    use_direct_paths = isTRUE(params$use_direct_paths)
  )
}

same_analysis_fingerprint <- function(x, y) {
  isTRUE(identical(x, y))
}

clear_proxy_contents <- function(proxy_dir) {
  if (!dir.exists(proxy_dir)) return(invisible(NULL))
  contents <- list.files(proxy_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  safe_unlink(contents)
  invisible(NULL)
}

prepare_proxy_inputs <- function(params, proxy_dir) {
  input_dir <- file.path(proxy_dir, "inputs")
  geo_dir <- file.path(input_dir, "geo")
  raster_dir <- file.path(input_dir, "rasters")
  safe_unlink(input_dir)
  dir.create(geo_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(raster_dir, recursive = TRUE, showWarnings = FALSE)

  geo_files <- copy_files_to_proxy(geo_bundle_files(params$geo_path), geo_dir)
  geo_primary <- geo_files[
    tolower(tools::file_ext(geo_files)) == tolower(tools::file_ext(params$geo_path)) &
      basename(geo_files) == basename(params$geo_path)
  ][1] %||% geo_files[[1]]

  raster_index <- raster_index_for_params(params)
  raster_index$path <- copy_files_to_proxy(raster_index$path, raster_dir)
  raster_index$file <- basename(raster_index$path)

  params$geo_path <- normalizePath(geo_primary, mustWork = TRUE)
  params$raster_folder <- normalizePath(raster_dir, mustWork = TRUE)
  params$raster_index <- raster_index
  params
}

prepare_direct_inputs <- function(params) {
  params$geo_path <- normalizePath(params$geo_path, mustWork = TRUE)
  params$raster_folder <- normalizePath(params$raster_folder, mustWork = TRUE)
  params$raster_index <- raster_index_for_params(params)
  params
}

read_validated_geo_checkpoint <- function(path) {
  if (is.null(path) || !length(path) || !file.exists(path[[1]])) return(NULL)
  saved <- tryCatch(readRDS(path[[1]]), error = function(e) NULL)
  geo <- if (is.list(saved) && inherits(saved$geo, "sf")) saved$geo else saved
  if (!inherits(geo, "sf") || !nrow(geo) || !isTRUE(attr(geo, "landscript_validated"))) {
    return(NULL)
  }
  read_geo(geo)
}

analysis_result_files_ready <- function(result) {
  if (is.null(result) || is.null(result$files)) return(FALSE)
  xlsx_files <- result$files["complete_xlsx"]
  gpkg_files <- result$files[grepl("gpkg", names(result$files))]
  all(!is.na(xlsx_files) & file.exists(xlsx_files)) &&
    length(gpkg_files) > 0L &&
    all(!is.na(gpkg_files) & file.exists(gpkg_files))
}

resolve_result_checkpoint <- function(checkpoint) {
  result_path <- checkpoint$result_rds %||% NULL
  if (!is.null(result_path) && length(result_path) && file.exists(result_path[[1]])) {
    restored <- tryCatch(readRDS(result_path[[1]]), error = function(e) NULL)
    if (!is.null(restored)) return(restored)
  }
  checkpoint
}

analysis_level_labels <- function() {
  c(
    grid = "Grid_ID",
    mesh = "Malha",
    groups = "Grupos",
    groups_1 = "Grupos 1",
    groups_2 = "Grupos 2",
    total = "Total"
  )
}

is_lazy_analysis_result <- function(result) {
  inherits(result, "landscript_lazy_result") || isTRUE(result$landscript_lazy)
}

analysis_result_levels <- function(result) {
  known <- names(analysis_level_labels())
  if (is_lazy_analysis_result(result)) {
    levels <- as.character(result$available_levels %||% character())
    return(known[known %in% levels])
  }
  known[vapply(known, function(level) !is.null(result[[level]]), logical(1))]
}

analysis_result_pointer <- function(result, result_path = NULL) {
  levels <- analysis_result_levels(result)
  row_counts <- vapply(levels, function(level) nrow(result[[level]]), numeric(1))
  group_columns <- normalize_group_columns(
    result$group_columns %||% result$group_column %||% character(),
    max_columns = 2L
  )
  group_count <- 1L
  if (length(group_columns) && !is.null(result$groups) &&
      all(group_columns %in% names(result$groups))) {
    group_count <- nrow(unique(sf::st_drop_geometry(result$groups)[group_columns]))
  }
  years <- suppressWarnings(as.integer(result$raster_index$year %||% integer()))
  years <- years[is.finite(years)]

  pointer <- list(
    landscript_lazy = TRUE,
    job_result_rds = if (!is.null(result_path) && length(result_path)) {
      normalizePath(result_path[[1]], mustWork = FALSE)
    } else {
      NULL
    },
    available_levels = levels,
    row_counts = row_counts,
    mesh_count = unname(row_counts[["mesh"]] %||% 0),
    group_count = as.integer(group_count),
    year_min = if (length(years)) min(years) else NA_integer_,
    year_max = if (length(years)) max(years) else NA_integer_,
    raster_count = if (is.null(result$raster_index)) 0L else nrow(result$raster_index),
    group_column = group_columns,
    group_columns = group_columns,
    summary_class_columns = result$summary_class_columns %||% character(),
    output_name = result$output_name,
    output_folder = result$output_folder,
    overlap_removed = isTRUE(result$overlap_removed),
    files = result$files
  )
  class(pointer) <- c("landscript_lazy_result", "list")
  pointer
}

analysis_result_layer_file <- function(result, level) {
  files <- result$files %||% character()
  if (!length(files)) return(NULL)

  preferred_key <- switch(
    level,
    mesh = "complete_gpkg_id_mesh",
    grid = "complete_gpkg_grid_id",
    groups = "complete_gpkg_grupos",
    groups_1 = "complete_gpkg_grupos",
    groups_2 = "complete_gpkg_grupos",
    total = "complete_gpkg_total",
    NULL
  )
  candidate_keys <- unique(c("complete_gpkg", preferred_key, names(files)[grepl("gpkg", names(files))]))
  candidates <- unname(files[candidate_keys[candidate_keys %in% names(files)]])
  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates) & file.exists(candidates)])

  for (path in candidates) {
    layers <- tryCatch(sf::st_layers(path)$name, error = function(e) character())
    if (level %in% layers) return(path)
  }
  NULL
}

load_analysis_result_level <- function(result, level) {
  levels <- analysis_result_levels(result)
  if (!length(levels)) stop("O resultado não possui camadas disponíveis.", call. = FALSE)
  if (!level %in% levels) {
    stop("A camada de resultado '", level, "' não está disponível.", call. = FALSE)
  }
  if (!is_lazy_analysis_result(result)) return(result[[level]])

  cache <- result$cache
  if (!is.environment(cache)) {
    cache <- new.env(parent = emptyenv())
  }
  if (exists(level, envir = cache, inherits = FALSE)) {
    return(get(level, envir = cache, inherits = FALSE))
  }

  gpkg <- analysis_result_layer_file(result, level)
  data <- if (!is.null(gpkg)) {
    read_result_dataset(gpkg, layer = level)
  } else {
    result_path <- result$job_result_rds %||% NULL
    if (is.null(result_path) || !length(result_path) || !file.exists(result_path[[1]])) {
      stop("Não foi possível localizar a camada '", level, "' nos arquivos finais.", call. = FALSE)
    }
    complete <- readRDS(result_path[[1]])
    complete[[level]]
  }
  if (is.null(data)) {
    stop("A camada de resultado '", level, "' está vazia.", call. = FALSE)
  }
  group_columns <- normalize_group_columns(
    result$group_columns %||% result$group_column %||% character(),
    max_columns = 2L
  )
  for (column in intersect(group_columns, names(data))) {
    data[[column]] <- factor(data[[column]])
  }
  assign(level, data, envir = cache)
  data
}

read_analysis_job_result <- function(path, lazy = FALSE) {
  payload <- readRDS(path)
  if (isTRUE(lazy)) {
    if (!is_lazy_analysis_result(payload)) {
      result_path <- payload$job_result_rds %||% NULL
      complete <- if (!is.null(result_path) && length(result_path) && file.exists(result_path[[1]])) {
        readRDS(result_path[[1]])
      } else {
        payload
      }
      payload <- analysis_result_pointer(complete, result_path)
    }
    payload$cache <- new.env(parent = emptyenv())
    return(payload)
  }

  result_path <- payload$job_result_rds %||% NULL
  if (!is.null(result_path) && length(result_path) && file.exists(result_path[[1]])) {
    return(readRDS(result_path[[1]]))
  }
  payload
}

run_land_analysis <- function(params, progress_file = NULL) {
  progress <- function(percent, stage, detail = NULL, status = "running") {
    if (!is.null(progress_file)) {
      write_progress(progress_file, percent, stage, detail, status)
    }
  }

  progress(2, "Validação", "Conferindo arquivos e parâmetros")
  output_folder <- ensure_output_folder(params$output_folder)
  output_name <- sanitize_output_name(params$output_name)
  params$output_folder <- output_folder
  params$output_name <- output_name
  proxy_dir <- analysis_proxy_dir(output_folder, output_name)
  dir.create(proxy_dir, recursive = TRUE, showWarnings = FALSE)

  prepared_params <- NULL
  metadata <- read_checkpoint(proxy_dir, "metadata")
  if (isTRUE(params$resume_proxy)) {
    prepared_params <- read_checkpoint(proxy_dir, "params")
    if (is.null(prepared_params)) {
      stop("Não há parâmetros salvos na pasta proxy para retomar a análise.", call. = FALSE)
    }
    params <- prepared_params
    output_folder <- params$output_folder
    output_name <- params$output_name
    progress(3, "Retomada", paste0("Usando pasta proxy: ", proxy_dir))
  } else {
    current_fingerprint <- analysis_fingerprint(params)
    if (!is.null(metadata) && same_analysis_fingerprint(metadata$fingerprint, current_fingerprint)) {
      prepared_params <- read_checkpoint(proxy_dir, "params")
      if (!is.null(prepared_params)) {
        params <- prepared_params
        progress(3, "Retomada", paste0("Checkpoints encontrados em ", proxy_dir))
      }
    }
    if (is.null(prepared_params)) {
      if (!is.null(metadata)) clear_proxy_contents(proxy_dir)
      dir.create(proxy_dir, recursive = TRUE, showWarnings = FALSE)
      if (isTRUE(params$use_direct_paths)) {
        progress(3, "Proxy", "Usando caminhos locais diretamente; sem copiar entradas para o proxy.")
        params <- prepare_direct_inputs(params)
      } else {
        progress(3, "Proxy", paste0("Copiando entradas para ", proxy_dir))
        params <- prepare_proxy_inputs(params, proxy_dir)
      }
      write_checkpoint(params, proxy_dir, "params")
      write_checkpoint(
        list(
          fingerprint = current_fingerprint,
          created_at = Sys.time(),
          proxy_dir = proxy_dir
        ),
        proxy_dir,
        "metadata"
      )
    }
  }

  # Keep terra's potentially large temporary files beside the selected output
  # and proxy folders. This avoids silently filling the system temporary disk
  # when the user deliberately selected a larger external drive.
  terra_tmp <- file.path(proxy_dir, "terra_tmp")
  dir.create(terra_tmp, recursive = TRUE, showWarnings = FALSE)
  terra::terraOptions(tempdir = terra_tmp, progress = 0)

  final_checkpoint <- read_checkpoint(proxy_dir, "result")
  if (analysis_result_files_ready(final_checkpoint)) {
    progress(98, "Retomada", "Resultado final encontrado; conferindo arquivos.")
    final_result <- resolve_result_checkpoint(final_checkpoint)
    progress(99, "Limpeza", "Removendo pasta proxy temporária.")
    safe_unlink(proxy_dir)
    progress(100, "Concluído", "Resultado retomado e pasta proxy removida.", "complete")
    return(final_result)
  }

  raster_index <- raster_index_for_params(params)

  progress(6, "Leitura", "Abrindo e validando o arquivo geoespacial")
  mesh_checkpoint <- read_checkpoint(proxy_dir, "mesh")
  if (!is.null(mesh_checkpoint)) {
    progress(14, "Retomada", "Usando limites e malha salvos na pasta proxy.")
    boundaries <- mesh_checkpoint$boundaries
    group_columns <- mesh_checkpoint$group_columns %||% mesh_checkpoint$group_column
    overlap_removed <- mesh_checkpoint$overlap_removed
    mesh <- mesh_checkpoint$mesh
  } else {
    geo <- read_validated_geo_checkpoint(params$validated_geo_rds)
    if (is.null(geo)) {
      validate_shapefile_geometry(params$geo_path)
      geo <- read_geo(params$geo_path)
    } else {
      progress(8, "Leitura", "Reutilizando a geometria já validada pelo dashboard")
    }
    group_columns <- normalize_group_columns(params$group_column %||% character(), max_columns = 2L)

    progress(10, "Limites", "Dissolvendo classes e removendo sobreposições")
    boundaries <- prepare_group_boundaries(geo, params$group_column)
    group_columns <- attr(boundaries, "group_columns") %||% attr(boundaries, "group_column")
    overlap_removed <- isTRUE(attr(boundaries, "overlap_removed"))

    progress(14, "Malha", "Criando e recortando a malha quadrangular")
    mesh <- create.mesh(
      boundaries,
      mesh.size = if (isTRUE(params$no_mesh)) NULL else params$mesh_size,
      group.column = group_columns,
      max.cells = params$max_cells %||% 50000L,
      mesh.unit = params$mesh_unit %||% "degrees",
      prepared.boundaries = TRUE
    )
    for (column in group_columns) {
      mesh[[column]] <- factor(mesh[[column]])
    }
    write_checkpoint(
      list(
        boundaries = boundaries,
        group_column = group_columns,
        group_columns = group_columns,
        overlap_removed = overlap_removed,
        mesh = mesh
      ),
      proxy_dir,
      "mesh"
    )
  }

  grid_sizes <- tabulate(raster_index$grid_group)
  aligned_grid_count <- length(grid_sizes)
  if (nrow(raster_index) > 1L && aligned_grid_count == 1L) {
    progress(
      14.5,
      "Alinhamento dos rasters",
      "Grades idênticas confirmadas; a rasterização da malha será reutilizada entre os anos."
    )
  } else if (nrow(raster_index) > 1L) {
    progress(
      14.5,
      "Alinhamento dos rasters",
      paste0(
        "Foram identificadas ", aligned_grid_count,
        " grades distintas; cada grade será processada de forma independente."
      )
    )
  }

  zone_cache <- new_zonal_cache()
  all_counts <- vector("list", nrow(raster_index))
  counts_dir <- file.path(proxy_dir, "raster_counts")
  dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(raster_index))) {
    pct <- 15 + 55 * (i - 1) / max(1, nrow(raster_index))
    count_checkpoint <- file.path(
      counts_dir,
      sprintf("%03d_%s_%s.rds", i, raster_index$year[[i]], tools::file_path_sans_ext(raster_index$file[[i]]))
    )
    if (file.exists(count_checkpoint)) {
      saved_count <- tryCatch(readRDS(count_checkpoint), error = function(e) NULL)
      if (!is.null(saved_count)) {
        all_counts[[i]] <- saved_count
        progress(
          pct,
          paste0("Raster ", i, " de ", nrow(raster_index)),
          paste0("Retomado do proxy: ", raster_index$year[[i]], " — ", raster_index$file[[i]])
        )
        next
      }
    }
    grid_group <- raster_index$grid_group[[i]]
    cache_key <- if (aligned_grid_count == 1L && nrow(raster_index) > 1L) {
      paste0("grid_", grid_group)
    } else {
      NULL
    }
    cache_available <- !is.null(cache_key) && !is.null(zone_cache$zones[[cache_key]])
    progress(
      pct,
      paste0("Raster ", i, " de ", nrow(raster_index)),
      paste0(
        raster_index$year[[i]], " — ", raster_index$file[[i]], ". ",
        if (cache_available) {
          "Reutilizando a malha rasterizada da grade alinhada."
        } else {
          "Limitando o processamento à extensão da área de estudo."
        }
      )
    )
    all_counts[[i]] <- extract_zonal_counts(
      raster_index$path[[i]],
      mesh,
      raster_index$year[[i]],
      zone_cache = zone_cache,
      cache_key = cache_key
    )
    saveRDS(all_counts[[i]], count_checkpoint)
  }

  if (zone_cache$source_cells > 0) {
    reduction <- 100 * (1 - zone_cache$processed_cells / zone_cache$source_cells)
    progress(
      70,
      "Rasters processados",
      paste0(
        "Recorte pela área de estudo reduziu em ",
        format(round(max(0, reduction), 1), decimal.mark = ",", trim = TRUE),
        "% as células avaliadas; ", zone_cache$rasterizations,
        " rasterização(ões) da malha e ", zone_cache$zone_cache_hits,
        " reutilização(ões)."
      )
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
  for (column in group_columns) {
    unit_table[[column]] <- factor(unit_table[[column]])
  }

  if (identical(params$mapbiomas, "none")) {
    progress(78, "Classes", "Mantendo as classes originais dos rasters")
  } else if (identical(params$mapbiomas, "custom")) {
    progress(78, "Classes", "Aplicando a legenda personalizada")
  } else {
    progress(78, "Classes", paste0("Aplicando a legenda MapBiomas Coleção ", params$mapbiomas))
  }
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

  progress(89, "Agregação", "Gerando resultados por quadrícula, grupo e área total")
  grid_results <- aggregate_grid_result(
    unit_table,
    mesh,
    value_columns = value_columns
  )

  group_geometry <- boundaries[, c(group_columns, "geometry")]
  group_results <- aggregate_result_level(
    unit_table,
    group_geometry,
    by_columns = group_columns,
    value_columns = value_columns,
    level_name = "Grupo"
  )

  group_level_results <- list()
  if (length(group_columns) > 1L) {
    for (i in seq_along(group_columns)) {
      group_level_geometry <- aggregate_boundary_geometry(boundaries, group_columns[[i]])
      group_level_results[[paste0("groups_", i)]] <- aggregate_result_level(
        unit_table,
        group_level_geometry,
        by_columns = group_columns[[i]],
        value_columns = value_columns,
        level_name = paste0("Grupo ", i)
      )
    }
  }

  total_geometry <- sf::st_sf(
    geometry = sf::st_sfc(safe_st_union(sf::st_geometry(boundaries)), crs = sf::st_crs(boundaries))
  )
  total_results <- aggregate_result_level(
    unit_table,
    total_geometry,
    by_columns = character(),
    value_columns = value_columns,
    level_name = "Total"
  )

  progress(94, "Exportação", "Salvando tabelas e GeoPackages")
  layers <- c(
    list(grid = grid_results, mesh = unit_results, groups = group_results),
    group_level_results,
    list(total = total_results)
  )
  complete_table <- dplyr::bind_rows(lapply(layers, sf::st_drop_geometry))

  complete_txt <- file.path(output_folder, paste0(output_name, ".txt"))
  complete_xlsx <- file.path(output_folder, paste0(output_name, ".xlsx"))
  pixels_txt <- file.path(output_folder, paste0(output_name, "_CalcRasterPixels.txt"))
  result_rds <- file.path(output_folder, paste0(output_name, ".rds"))

  utils::write.table(
    complete_table, complete_txt, sep = "\t", dec = ".", quote = FALSE,
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  utils::write.table(
    counts, pixels_txt, sep = "\t", dec = ".", quote = FALSE,
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  write_result_workbook(complete_xlsx, layers)
  gpkg_files <- write_result_geopackages(output_folder, output_name, layers)

  result <- list(
    grid = grid_results,
    mesh = unit_results,
    groups = group_results,
    groups_1 = group_level_results[["groups_1"]],
    groups_2 = group_level_results[["groups_2"]],
    total = total_results,
    complete_table = complete_table,
    raster_index = raster_index,
    group_column = group_columns,
    group_columns = group_columns,
    summary_class_columns = summary_class_columns,
    output_name = output_name,
    output_folder = output_folder,
    overlap_removed = overlap_removed,
    files = c(
      complete_table = normalizePath(complete_txt),
      complete_xlsx = normalizePath(complete_xlsx),
      gpkg_files,
      pixel_counts = normalizePath(pixels_txt),
      result_rds = normalizePath(result_rds, mustWork = FALSE)
    )
  )
  save_rds_atomic(result, result_rds)
  write_checkpoint(
    list(
      files = result$files,
      result_rds = normalizePath(result_rds, mustWork = TRUE)
    ),
    proxy_dir,
    "result"
  )

  progress(99, "Limpeza", "Removendo pasta proxy temporária.")
  safe_unlink(proxy_dir)
  progress(100, "Concluído", "Análise finalizada e arquivos exportados.", "complete")
  result
}

run_analysis_job <- function(params, progress_file, result_file, app_directory) {
  options(landscript.app_dir = app_directory)

  tryCatch({
    result <- run_land_analysis(params, progress_file)
    result_path <- result$files[["result_rds"]] %||% NULL
    pointer <- analysis_result_pointer(result, result_path)
    save_rds_atomic(pointer, result_file)
    invisible(result_file)
  }, error = function(e) {
    write_progress(progress_file, 100, "Erro", conditionMessage(e), "error")
    stop(e)
  })
}
