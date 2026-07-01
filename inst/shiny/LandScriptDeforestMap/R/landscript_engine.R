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

result_excel_sheets <- function(layers, simplified = FALSE) {
  sheet_names <- c(
    mesh = "Malha",
    groups = "Grupos",
    total = "Total"
  )
  sheets <- lapply(layers, function(object) {
    if (simplified) object <- drop_raw_class_columns(object)
    sf::st_drop_geometry(object)
  })
  names(sheets) <- unname(sheet_names[names(sheets)] %||% names(sheets))
  sheets
}

write_result_workbook <- function(path, layers, simplified = FALSE) {
  if (file.exists(path)) unlink(path)
  writexl::write_xlsx(
    result_excel_sheets(layers, simplified = simplified),
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

analysis_fingerprint <- function(params) {
  geo_files <- geo_bundle_files(params$geo_path)
  geo_info <- file.info(geo_files)
  raster_index <- list_raster_files(params$raster_folder)
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
    group_column = params$group_column %||% "",
    no_mesh = isTRUE(params$no_mesh),
    mesh_size = params$mesh_size %||% NA_real_,
    mesh_unit = params$mesh_unit %||% "degrees",
    mapbiomas = params$mapbiomas %||% "",
    custom_classes = params$custom_classes,
    pixel_km2_ratio = params$pixel_km2_ratio,
    max_cells = params$max_cells %||% NA_integer_
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

  raster_index <- list_raster_files(params$raster_folder)
  copy_files_to_proxy(raster_index$path, raster_dir)

  params$geo_path <- normalizePath(geo_primary, mustWork = TRUE)
  params$raster_folder <- normalizePath(raster_dir, mustWork = TRUE)
  params
}

analysis_result_files_ready <- function(result) {
  if (is.null(result) || is.null(result$files)) return(FALSE)
  required <- c("complete_xlsx", "simplified_xlsx", "complete_gpkg", "simplified_gpkg")
  files <- result$files[required]
  all(!is.na(files) & file.exists(files))
}

ensure_result_zip <- function(result) {
  zip_path <- file.path(result$output_folder, paste0(result$output_name, "_resultados.zip"))
  prepare_result_download_zip(result, zip_path)
  result$files["result_zip"] <- normalizePath(zip_path, mustWork = TRUE)
  result
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
      progress(3, "Proxy", paste0("Copiando entradas para ", proxy_dir))
      params <- prepare_proxy_inputs(params, proxy_dir)
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

  final_checkpoint <- read_checkpoint(proxy_dir, "result")
  if (analysis_result_files_ready(final_checkpoint)) {
    progress(98, "Retomada", "Resultado final encontrado; conferindo ZIP.")
    final_checkpoint <- ensure_result_zip(final_checkpoint)
    write_checkpoint(final_checkpoint, proxy_dir, "result")
    progress(99, "Limpeza", "Removendo pasta proxy temporária.")
    safe_unlink(proxy_dir)
    progress(100, "Concluído", "Resultado retomado, ZIP salvo e pasta proxy removida.", "complete")
    return(final_checkpoint)
  }

  raster_index <- list_raster_files(params$raster_folder)

  progress(6, "Leitura", "Abrindo e validando o arquivo geoespacial")
  mesh_checkpoint <- read_checkpoint(proxy_dir, "mesh")
  if (!is.null(mesh_checkpoint)) {
    progress(14, "Retomada", "Usando limites e malha salvos na pasta proxy.")
    boundaries <- mesh_checkpoint$boundaries
    group_column <- mesh_checkpoint$group_column
    overlap_removed <- mesh_checkpoint$overlap_removed
    mesh <- mesh_checkpoint$mesh
  } else {
    validate_shapefile_geometry(params$geo_path)
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
    write_checkpoint(
      list(
        boundaries = boundaries,
        group_column = group_column,
        overlap_removed = overlap_removed,
        mesh = mesh
      ),
      proxy_dir,
      "mesh"
    )
  }

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
    saveRDS(all_counts[[i]], count_checkpoint)
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

  progress(94, "Exportação", "Salvando tabelas e GeoPackages")
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
  write_result_workbook(complete_xlsx, layers, simplified = FALSE)
  write_result_workbook(simplified_xlsx, layers, simplified = TRUE)

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
  result <- ensure_result_zip(result)
  saveRDS(result, result_rds)
  write_checkpoint(result, proxy_dir, "result")

  progress(99, "Limpeza", "Removendo pasta proxy temporária.")
  safe_unlink(proxy_dir)
  progress(100, "Concluído", "Análise finalizada, arquivos exportados e ZIP salvo.", "complete")
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
