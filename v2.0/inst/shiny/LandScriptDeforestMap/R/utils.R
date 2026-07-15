`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) return(y)
  # Only inspect missingness for atomic vectors. Calling is.na()/all() on sf,
  # terra or other large structured objects can scan the whole object and may
  # even dispatch to an incompatible method merely to decide a fallback.
  if (is.atomic(x) && all(is.na(x))) y else x
}

app_root <- function() {
  normalizePath(getOption("landscript.app_dir", getwd()), mustWork = FALSE)
}

required_packages <- function() {
  c(
    "shiny", "bslib", "leaflet", "sf", "terra", "dplyr", "tidyr",
    "ggplot2", "ggspatial", "DT", "callr", "processx", "jsonlite",
    "zip", "htmltools", "scales", "shinyFiles", "writexl", "readxl", "rosm",
    "prettymapr", "later"
  )
}

check_required_packages <- function(stop_on_missing = TRUE) {
  missing <- required_packages()[
    !vapply(required_packages(), requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) && stop_on_missing) {
    stop(
      "Pacotes R ausentes: ", paste(missing, collapse = ", "),
      ". Execute install_dependencies.R antes de iniciar a aplicação.",
      call. = FALSE
    )
  }

  missing
}

format_bytes <- function(bytes) {
  if (is.null(bytes) || is.na(bytes)) return("—")
  units <- c("B", "KB", "MB", "GB", "TB")
  power <- if (bytes > 0) min(floor(log(bytes, 1024)), length(units) - 1L) else 0L
  paste0(round(bytes / (1024^power), 1), " ", units[power + 1L])
}

sanitize_output_name <- function(x) {
  x <- trimws(x %||% "")
  x <- tools::file_path_sans_ext(basename(x))
  x <- gsub("[^[:alnum:]_.-]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) "LandScript_result" else x
}

parse_number_vector <- function(x, integer = FALSE, allow_empty = TRUE) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) {
    if (allow_empty) return(numeric())
    stop("Informe ao menos um valor.", call. = FALSE)
  }

  values <- trimws(unlist(strsplit(x, "[,;[:space:]]+")))
  values <- values[nzchar(values)]
  out <- suppressWarnings(as.numeric(values))

  if (anyNA(out)) {
    stop("Há valores não numéricos na lista: ", x, call. = FALSE)
  }

  if (integer) out <- as.integer(out)
  unique(out)
}

parse_color_vector <- function(x, minimum = 1L) {
  colors <- trimws(unlist(strsplit(x %||% "", "[,;]+")))
  colors <- colors[nzchar(colors)]
  if (length(colors) < minimum) {
    stop("Informe ao menos ", minimum, " cor(es), separadas por vírgula.", call. = FALSE)
  }
  invalid <- !vapply(colors, function(z) {
    tryCatch({
      grDevices::col2rgb(z)
      TRUE
    }, error = function(e) FALSE)
  }, logical(1))
  if (any(invalid)) {
    stop("Cor(es) inválida(s): ", paste(colors[invalid], collapse = ", "), call. = FALSE)
  }
  colors
}

extract_year_candidates_from_text <- function(text) {
  text <- as.character(text %||% "")
  hits <- gregexpr("(?<![0-9])(?:19|20)[0-9]{2}(?![0-9])", text, perl = TRUE)[[1]]
  if (length(hits) == 1L && hits[[1]] == -1L) {
    return(data.frame(start = integer(), end = integer(), year = integer()))
  }
  values <- regmatches(text, list(hits))[[1]]
  data.frame(
    start = as.integer(hits),
    end = as.integer(hits + attr(hits, "match.length") - 1L),
    year = as.integer(values),
    stringsAsFactors = FALSE
  )
}

extract_year_from_metadata <- function(path) {
  out <- tryCatch({
    raster <- terra::rast(path)
    raster_time <- terra::time(raster)
    if (length(raster_time) && !all(is.na(raster_time))) {
      if (inherits(raster_time, c("Date", "POSIXct", "POSIXt"))) {
        year <- as.integer(format(raster_time[[1]], "%Y"))
      } else {
        year <- suppressWarnings(as.integer(raster_time[[1]]))
      }
      if (is.finite(year) && year >= 1900L && year <= 2099L) {
        return(year)
      }
    }

    layer_candidates <- extract_year_candidates_from_text(paste(names(raster), collapse = " "))
    if (nrow(layer_candidates)) {
      return(layer_candidates$year[[nrow(layer_candidates)]])
    }
    NA_integer_
  }, error = function(e) NA_integer_)
  as.integer(out %||% NA_integer_)
}

extract_years_from_names <- function(files) {
  stems <- tools::file_path_sans_ext(basename(files))

  trailing <- vapply(stems, function(name) {
    hit <- regmatches(name, regexpr("(?:19|20)[0-9]{2}$", name, perl = TRUE))
    if (!length(hit) || identical(hit, character(0))) NA_integer_ else as.integer(hit)
  }, integer(1))
  if (!anyNA(trailing)) return(trailing)

  candidates <- lapply(stems, extract_year_candidates_from_text)
  common_positions <- Reduce(
    intersect,
    lapply(candidates, function(x) unique(x$start))
  )
  if (length(common_positions)) {
    scored <- lapply(common_positions, function(position) {
      years <- vapply(candidates, function(x) {
        hit <- x[x$start == position, , drop = FALSE]
        if (!nrow(hit)) NA_integer_ else hit$year[[1]]
      }, integer(1))
      data.frame(
        position = position,
        duplicated = anyDuplicated(years) > 0L,
        missing = anyNA(years),
        year_span = if (anyNA(years)) Inf else diff(range(years)),
        years = I(list(years))
      )
    })
    scored <- do.call(rbind, scored)
    scored <- scored[!scored$missing, , drop = FALSE]
    if (nrow(scored)) {
      no_duplicates <- scored[!scored$duplicated, , drop = FALSE]
      selected <- if (nrow(no_duplicates)) {
        no_duplicates[order(no_duplicates$year_span, no_duplicates$position), , drop = FALSE][1, ]
      } else {
        scored[order(scored$year_span, scored$position), , drop = FALSE][1, ]
      }
      return(as.integer(selected$years[[1]]))
    }
  }

  rep(NA_integer_, length(files))
}

list_raster_files <- function(folder) {
  if (is.null(folder) || !nzchar(trimws(folder))) {
    stop("Informe a pasta que contém os rasters classificados.", call. = FALSE)
  }

  folder <- path.expand(trimws(folder))
  if (!dir.exists(folder)) {
    stop("A pasta de rasters não existe: ", folder, call. = FALSE)
  }

  files <- list.files(
    folder,
    pattern = "\\.(tif|tiff|img|vrt|grd)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  # macOS may create AppleDouble sidecar files (._name.tif) on external
  # volumes. They are metadata, not rasters, and opening them wastes time or
  # produces duplicate-year/invalid-raster errors.
  files <- files[!startsWith(basename(files), "._")]
  if (!length(files)) {
    stop("Nenhum raster suportado foi encontrado na pasta.", call. = FALSE)
  }

  years <- extract_years_from_names(files)
  if (anyNA(years)) {
    metadata_years <- vapply(files[is.na(years)], extract_year_from_metadata, integer(1))
    years[is.na(years)] <- metadata_years
  }
  years <- unname(as.integer(years))
  if (anyNA(years)) {
    stop(
      "anos não encontrados. Não foi possível identificar o ano no nome ou nos metadados de: ",
      paste(basename(files[is.na(years)]), collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(years)) {
    duplicated_years <- unique(years[duplicated(years)])
    stop(
      "Há mais de um raster para o(s) ano(s): ",
      paste(duplicated_years, collapse = ", "),
      ". Renomeie ou remova as duplicatas.",
      call. = FALSE
    )
  }

  data.frame(
    path = normalizePath(files, mustWork = TRUE),
    file = basename(files),
    year = years,
    stringsAsFactors = FALSE
  )[order(years), , drop = FALSE]
}

raster_pixel_area_km2 <- function(path) {
  raster <- terra::rast(path)
  if (terra::nlyr(raster) != 1L) {
    stop("O raster deve possuir uma única camada: ", basename(path), call. = FALSE)
  }
  if (!nzchar(terra::crs(raster))) {
    stop("Raster sem CRS: ", basename(path), call. = FALSE)
  }

  raster_pixel_area_from_raster(raster, basename(path))
}

raster_pixel_area_from_raster <- function(raster, label = "raster") {
  # cellSize(raster) materializes an area raster with the same number of cells
  # as the source. For continental rasters this can require tens of gigabytes,
  # even though the dashboard only needs the area of the central pixel. Build
  # an equivalent one-cell raster instead; terra then performs the same
  # geodesic calculation at constant cost.
  center <- cbind(
    mean(c(terra::xmin(raster), terra::xmax(raster))),
    mean(c(terra::ymin(raster), terra::ymax(raster)))
  )
  center_cell <- terra::cellFromXY(raster, center)[[1]]
  if (!is.finite(center_cell)) {
    stop("Não foi possível localizar o pixel central de ", label, ".", call. = FALSE)
  }
  center_xy <- terra::xyFromCell(raster, center_cell)
  resolution <- abs(terra::res(raster))
  one_cell <- terra::rast(
    nrows = 1,
    ncols = 1,
    xmin = center_xy[[1]] - resolution[[1]] / 2,
    xmax = center_xy[[1]] + resolution[[1]] / 2,
    ymin = center_xy[[2]] - resolution[[2]] / 2,
    ymax = center_xy[[2]] + resolution[[2]] / 2,
    crs = terra::crs(raster)
  )
  area <- terra::values(terra::cellSize(one_cell, unit = "km"))[[1]]
  if (!length(area) || !is.finite(area) || area <= 0) {
    stop("Não foi possível calcular a área do pixel de ", label, ".", call. = FALSE)
  }
  as.numeric(area)
}

normalize_pixel_area_km2 <- function(area, tolerance = 0.05) {
  if (length(area) != 1L || !is.finite(area) || area <= 0) {
    stop("A área do pixel deve ser um número positivo.", call. = FALSE)
  }

  # Áreas usuais, em km², para pixels quadrados. Por exemplo:
  # 30 m × 30 m = 900 m² = 0,0009 km².
  standard_areas <- c(
    `10 × 10 m` = 0.0001,
    `20 × 20 m` = 0.0004,
    `30 × 30 m` = 0.0009,
    `≈ 94,9 × 94,9 m (9.000 m²)` = 0.009,
    `100 × 100 m` = 0.01
  )
  relative_difference <- abs(standard_areas - area) / standard_areas
  closest <- which.min(relative_difference)

  if (relative_difference[[closest]] <= tolerance) {
    return(list(
      raw = area,
      value = unname(standard_areas[[closest]]),
      standardized = TRUE,
      standard_label = names(standard_areas)[[closest]],
      relative_difference = relative_difference[[closest]]
    ))
  }

  list(
    raw = area,
    value = area,
    standardized = FALSE,
    standard_label = NULL,
    relative_difference = NA_real_
  )
}

report_progress <- function(progress, percent, stage, detail = NULL, status = "running") {
  if (is.function(progress)) {
    progress(percent, stage, detail %||% "", status)
  }
}

estimate_remaining_processing_seconds <- function(
  completed_raster_seconds,
  remaining_rasters,
  raster_safety_multiplier = 1.10,
  postprocess_multiplier = 1
) {
  durations <- as.numeric(completed_raster_seconds)
  durations <- durations[is.finite(durations) & durations > 0]
  remaining_rasters <- suppressWarnings(as.integer(remaining_rasters))
  if (!length(durations) || length(remaining_rasters) != 1L ||
      !is.finite(remaining_rasters) || remaining_rasters < 0) {
    return(NA_real_)
  }

  recent <- utils::tail(durations, min(3L, length(durations)))
  representative <- max(mean(durations), mean(recent))
  postprocess_seconds <- max(30, representative * postprocess_multiplier)
  representative * remaining_rasters * raster_safety_multiplier + postprocess_seconds
}

format_processing_time <- function(seconds) {
  seconds <- suppressWarnings(as.numeric(seconds))
  if (length(seconds) != 1L || !is.finite(seconds) || seconds < 0) return(NULL)

  minutes <- seconds / 60
  if (minutes <= 59) {
    return(paste0(formatC(minutes, format = "f", digits = 1, decimal.mark = ","), " min"))
  }
  paste0(formatC(minutes / 60, format = "f", digits = 1, decimal.mark = ","), " h")
}

normalize_raster_crs_signature <- function(crs) {
  gsub("[[:space:]]+", "", as.character(crs %||% ""))
}

raster_grid_definition <- function(raster) {
  if (!inherits(raster, "SpatRaster")) {
    raster <- terra::rast(raster)
  }
  list(
    crs = normalize_raster_crs_signature(terra::crs(raster)),
    resolution = as.numeric(terra::res(raster)),
    extent = as.numeric(as.vector(terra::ext(raster))),
    origin = as.numeric(terra::origin(raster)),
    dimensions = c(terra::ncol(raster), terra::nrow(raster)),
    rotated = isTRUE(terra::is.rotated(raster))
  )
}

raster_grid_signature <- function(raster) {
  definition <- raster_grid_definition(raster)
  number_signature <- function(x) {
    paste(sprintf("%.17g", as.numeric(x)), collapse = ",")
  }
  paste(
    paste0("crs=", definition$crs),
    paste0("res=", number_signature(definition$resolution)),
    paste0("ext=", number_signature(definition$extent)),
    paste0("origin=", number_signature(definition$origin)),
    paste0("dim=", number_signature(definition$dimensions)),
    paste0("rotated=", as.integer(definition$rotated)),
    sep = "|"
  )
}

ensure_raster_grid_signatures <- function(index) {
  if (!is.data.frame(index) || !nrow(index) || !"path" %in% names(index)) {
    stop("O índice de rasters não é válido.", call. = FALSE)
  }

  signatures <- if ("grid_signature" %in% names(index)) {
    as.character(index$grid_signature)
  } else {
    rep(NA_character_, nrow(index))
  }
  missing <- is.na(signatures) | !nzchar(signatures)
  if (any(missing)) {
    signatures[missing] <- vapply(
      index$path[missing],
      function(path) raster_grid_signature(terra::rast(path)),
      character(1)
    )
  }
  index$grid_signature <- signatures
  index$grid_group <- match(signatures, unique(signatures))
  index
}

inspect_raster_folder <- function(folder, progress = NULL) {
  report_progress(progress, 2, "Carregando arquivos", "Procurando rasters na pasta selecionada.")
  index <- list_raster_files(folder)
  total <- nrow(index)
  report_progress(
    progress,
    5,
    "Carregando arquivos",
    paste0("Foram encontrados ", total, " arquivo(s) raster. Iniciando leitura.")
  )
  metadata <- vector("list", total)

  for (i in seq_len(total)) {
    path <- index$path[[i]]
    label <- paste0(index$year[[i]], " — ", basename(path))
    segment_start <- 5 + ((i - 1) / total) * 87
    segment_mid <- segment_start + (87 / total) * 0.4
    segment_end <- 5 + (i / total) * 87
    report_progress(
      progress,
      segment_start,
      paste0("Acessando imagem ", i, " de ", total),
      paste0(index$year[[i]], " — ", basename(path))
    )
    raster <- terra::rast(path)
    if (terra::nlyr(raster) != 1L) {
      stop("O raster deve possuir uma única camada: ", basename(path), call. = FALSE)
    }
    crs <- terra::crs(raster)
    if (!nzchar(crs)) stop("Raster sem CRS: ", basename(path), call. = FALSE)

    report_progress(
      progress,
      segment_mid,
      paste0("Validando imagem ", i, " de ", total),
      paste0(index$year[[i]], " — conferindo CRS, grade e área do pixel.")
    )

    grid_definition <- raster_grid_definition(raster)
    metadata[[i]] <- list(
      crs = crs,
      crs_label = terra::crs(raster, proj = TRUE),
      resolution = grid_definition$resolution,
      extent = grid_definition$extent,
      origin = grid_definition$origin,
      dimensions = grid_definition$dimensions,
      rotated = grid_definition$rotated,
      grid_signature = raster_grid_signature(raster),
      pixel_area_km2 = raster_pixel_area_from_raster(raster, label)
    )

    report_progress(
      progress,
      segment_end,
      paste0("Validando imagem ", i, " de ", total),
      paste0("Imagem validada: ", index$year[[i]], " — ", basename(path))
    )
  }

  report_progress(
    progress,
    92,
    "Consolidando validação",
    "Comparando CRS, resolução, extensão, origem e dimensões entre as imagens."
  )

  crs_values <- vapply(metadata, `[[`, character(1), "crs")
  resolution_values <- do.call(rbind, lapply(metadata, `[[`, "resolution"))
  dimension_values <- do.call(rbind, lapply(metadata, `[[`, "dimensions"))
  pixel_areas <- vapply(metadata, `[[`, numeric(1), "pixel_area_km2")
  normalized_area <- normalize_pixel_area_km2(stats::median(pixel_areas))
  same_crs <- length(unique(crs_values)) == 1L
  same_resolution <- all(apply(
    resolution_values,
    2,
    function(x) isTRUE(all.equal(x, rep(x[[1]], length(x)), tolerance = 1e-8))
  ))

  grid_signatures <- vapply(metadata, `[[`, character(1), "grid_signature")
  same_grid <- length(unique(grid_signatures)) == 1L
  index$pixel_area_km2 <- pixel_areas
  index$grid_signature <- grid_signatures
  index$grid_group <- match(grid_signatures, unique(grid_signatures))
  list(
    index = index,
    count = nrow(index),
    years = range(index$year),
    crs_label = metadata[[1]]$crs_label,
    resolution = metadata[[1]]$resolution,
    pixel_area_km2_raw = stats::median(pixel_areas),
    pixel_area_km2 = normalized_area$value,
    pixel_area_standardized = normalized_area$standardized,
    pixel_area_standard_label = normalized_area$standard_label,
    pixel_area_range = range(pixel_areas),
    estimated_uncompressed_zone_bytes = max(
      apply(dimension_values, 1, prod) * 4,
      na.rm = TRUE
    ),
    same_crs = same_crs,
    same_resolution = same_resolution,
    grids_aligned = same_grid,
    grid_group_count = length(unique(grid_signatures))
  )
}

nearest_existing_path <- function(path) {
  path <- path.expand(as.character(path %||% ""))
  if (!nzchar(path)) return(NA_character_)
  if (file.exists(path) && !dir.exists(path)) path <- dirname(path)
  while (!dir.exists(path)) {
    parent <- dirname(path)
    if (identical(parent, path)) return(NA_character_)
    path <- parent
  }
  normalizePath(path, mustWork = TRUE)
}

disk_available_bytes <- function(path) {
  path <- nearest_existing_path(path)
  if (is.na(path) || !nzchar(path)) return(NA_real_)

  if (.Platform$OS.type == "windows") {
    normalized <- normalizePath(path, winslash = "\\", mustWork = TRUE)
    drive <- sub("^([A-Za-z]):.*$", "\\1", normalized)
    if (!grepl("^[A-Za-z]$", drive)) return(NA_real_)
    command <- paste0("(Get-PSDrive -Name '", drive, "').Free")
    output <- tryCatch(
      system2(
        "powershell",
        c("-NoProfile", "-Command", shQuote(command)),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(e) character()
    )
    first_value <- if (length(output)) output[[1]] else NA_character_
    value <- suppressWarnings(as.numeric(trimws(first_value)))
    return(if (is.finite(value)) value else NA_real_)
  }

  output <- tryCatch(
    system2("df", c("-Pk", shQuote(path)), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(output) < 2L) return(NA_real_)
  fields <- strsplit(trimws(utils::tail(output, 1L)), "[[:space:]]+")[[1]]
  if (length(fields) < 4L) return(NA_real_)
  available_kb <- suppressWarnings(as.numeric(fields[[4]]))
  if (!is.finite(available_kb)) return(NA_real_)
  available_kb * 1024
}

disk_space_advisory <- function(inspection, work_directory) {
  needed <- suppressWarnings(as.numeric(
    inspection$estimated_uncompressed_zone_bytes %||% NA_real_
  ))
  available <- disk_available_bytes(work_directory)
  if (!is.finite(needed) || needed <= 0 || !is.finite(available) || available <= 0 || needed <= available) {
    return(NULL)
  }

  list(
    needed_bytes = needed,
    available_bytes = available,
    directory = nearest_existing_path(work_directory),
    text = paste0(
      "Espaço em disco possivelmente insuficiente: o processamento pode precisar de aproximadamente ",
      format_bytes(needed),
      " de espaço temporário sem compressão, mas há ",
      format_bytes(available),
      " disponíveis no disco usado pela pasta de saída. A execução continuará, mas pode ficar mais lenta ou apresentar erros."
    )
  )
}

ensure_output_folder <- function(folder) {
  if (is.null(folder) || !nzchar(trimws(folder))) {
    stop("Informe a pasta de saída.", call. = FALSE)
  }

  folder <- path.expand(trimws(folder))
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(folder)) {
    stop("Não foi possível criar a pasta de saída: ", folder, call. = FALSE)
  }

  probe <- tempfile("landscript_write_test_", tmpdir = folder)
  ok <- tryCatch({
    writeLines("ok", probe)
    unlink(probe)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    stop("A pasta de saída não possui permissão de escrita: ", folder, call. = FALSE)
  }

  normalizePath(folder, mustWork = TRUE)
}

write_progress <- function(
  path,
  percent,
  stage,
  detail = NULL,
  status = "running",
  eta_status = NULL,
  eta_seconds = NULL
) {
  payload <- list(
    percent = max(0, min(100, as.numeric(percent))),
    stage = as.character(stage),
    detail = as.character(detail %||% ""),
    status = status,
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  if (!is.null(eta_status)) payload$eta_status <- as.character(eta_status)
  if (!is.null(eta_seconds)) {
    eta_seconds <- suppressWarnings(as.numeric(eta_seconds))
    if (length(eta_seconds) == 1L && is.finite(eta_seconds)) {
      payload$eta_seconds <- max(0, eta_seconds)
    }
  }
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(payload, tmp, auto_unbox = TRUE, pretty = FALSE)
  moved <- file.rename(tmp, path)
  if (!isTRUE(moved)) {
    if (file.exists(path)) unlink(path)
    moved <- file.rename(tmp, path)
  }
  if (!isTRUE(moved)) {
    copied <- file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
    if (!isTRUE(copied)) {
      stop("Não foi possível atualizar o arquivo de progresso.", call. = FALSE)
    }
  }
  invisible(payload)
}

save_rds_atomic <- function(object, path) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(basename(path), "_"), tmpdir = directory, fileext = ".tmp")
  on.exit(safe_unlink(tmp), add = TRUE)
  saveRDS(object, tmp)
  if (file.exists(path)) unlink(path)
  moved <- file.rename(tmp, path)
  if (!isTRUE(moved)) {
    copied <- file.copy(tmp, path, overwrite = TRUE, copy.mode = FALSE)
    if (!isTRUE(copied)) {
      stop("Não foi possível salvar o arquivo temporário: ", basename(path), call. = FALSE)
    }
  }
  normalizePath(path, mustWork = TRUE)
}

read_progress <- function(path) {
  if (!file.exists(path)) {
    return(list(percent = 0, stage = "Preparando", detail = "", status = "starting"))
  }
  tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) list(percent = 0, stage = "Preparando", detail = "", status = "starting")
  )
}

supported_vector_extensions <- function() {
  c("gpkg", "geojson", "json", "shp", "kml", "gml")
}

shapefile_extensions <- function() {
  c("shp", "shx", "dbf", "prj", "cpg", "qpj", "sbn", "sbx", "xml")
}

required_shapefile_extensions <- function() {
  c("shp", "shx", "dbf", "prj")
}

select_primary_vector_file <- function(files) {
  files <- normalizePath(files[file.exists(files)], mustWork = TRUE)
  extensions <- tolower(tools::file_ext(files))
  priority <- c("gpkg", "geojson", "json", "shp", "kml", "gml")

  for (ext in priority) {
    candidate <- files[extensions == ext]
    if (length(candidate)) return(candidate[[1]])
  }

  stop(
    "Formato não reconhecido. Envie GeoPackage, GeoJSON, ZIP com shapefile completo ou arquivo suportado pelo sf.",
    call. = FALSE
  )
}

complete_shapefile_bundle <- function(primary, destination, uploaded_files = character(), source = c("upload", "local", "zip")) {
  source <- match.arg(source)
  primary <- normalizePath(primary, mustWork = TRUE)
  primary_base <- tolower(tools::file_path_sans_ext(basename(primary)))
  destination <- normalizePath(destination, mustWork = FALSE)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)

  uploaded_files <- uploaded_files[file.exists(uploaded_files)]
  sibling_files <- list.files(dirname(primary), full.names = TRUE, recursive = FALSE)
  candidates <- unique(c(primary, uploaded_files, sibling_files))
  candidate_base <- tolower(tools::file_path_sans_ext(basename(candidates)))
  candidate_ext <- tolower(tools::file_ext(candidates))
  selected <- candidates[
    candidate_base == primary_base &
      candidate_ext %in% shapefile_extensions()
  ]

  available_extensions <- unique(tolower(tools::file_ext(selected)))
  missing_extensions <- setdiff(required_shapefile_extensions(), available_extensions)

  if (length(missing_extensions)) {
    source_hint <- switch(
      source,
      local = "Confira se esses arquivos estão no mesmo diretório do .shp selecionado.",
      zip = "Confira se o ZIP contém todos os componentes do shapefile.",
      upload = paste0(
        "No upload pelo navegador, o Shiny só recebe os arquivos selecionados. ",
        "Envie um .zip com todos os componentes ou selecione o .shp, .shx, .dbf ",
        "e .prj juntos."
      )
    )
    stop(
      "O shapefile está incompleto. São necessários arquivos com o mesmo nome-base: ",
      paste0(".", required_shapefile_extensions(), collapse = ", "),
      ". Está faltando: ",
      paste0(".", missing_extensions, collapse = ", "),
      ". ",
      source_hint,
      call. = FALSE
    )
  }

  target <- file.path(destination, basename(selected))
  copied_ok <- vapply(seq_along(selected), function(i) {
    source_path <- normalizePath(selected[[i]], mustWork = TRUE)
    target_path <- normalizePath(target[[i]], mustWork = FALSE)
    if (identical(source_path, target_path)) return(TRUE)
    file.copy(source_path, target[[i]], overwrite = TRUE)
  }, logical(1))
  if (!all(copied_ok)) {
    stop("Não foi possível preparar os arquivos do shapefile.", call. = FALSE)
  }
  normalizePath(target[tolower(tools::file_ext(target)) == "shp"][[1]], mustWork = TRUE)
}

stage_vector_files <- function(files, destination, source = c("upload", "local", "zip")) {
  source <- match.arg(source)
  files <- normalizePath(files[file.exists(files)], mustWork = TRUE)
  primary <- select_primary_vector_file(files)

  if (tolower(tools::file_ext(primary)) == "shp") {
    return(complete_shapefile_bundle(primary, destination, uploaded_files = files, source = source))
  }

  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(destination, basename(primary))
  if (identical(normalizePath(primary, mustWork = TRUE), normalizePath(target, mustWork = FALSE))) {
    return(normalizePath(primary, mustWork = TRUE))
  }
  if (!file.copy(primary, target, overwrite = TRUE)) {
    stop("Não foi possível preparar o arquivo geoespacial.", call. = FALSE)
  }
  normalizePath(target, mustWork = TRUE)
}

stage_local_vector <- function(path, destination) {
  if (is.null(path) || !length(path) || !file.exists(path[[1]])) {
    stop("Selecione um arquivo geoespacial.", call. = FALSE)
  }

  path <- normalizePath(path[[1]], mustWork = TRUE)
  if (tolower(tools::file_ext(path)) == "zip") {
    extracted <- safe_extract_zip(path, file.path(destination, "zip_extracted"))
    return(stage_vector_files(extracted, destination, source = "zip"))
  }

  stage_vector_files(path, destination, source = "local")
}

stage_uploaded_vector <- function(upload, destination) {
  if (is.null(upload) || !nrow(upload)) {
    stop("Selecione um arquivo geoespacial.", call. = FALSE)
  }

  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  copied <- file.path(destination, basename(upload$name))
  ok <- file.copy(upload$datapath, copied, overwrite = TRUE)
  if (!all(ok)) stop("Não foi possível preparar os arquivos enviados.", call. = FALSE)

  zip_files <- copied[tolower(tools::file_ext(copied)) == "zip"]
  if (length(zip_files)) {
    extracted <- safe_extract_zip(zip_files[[1]], file.path(destination, "zip_extracted"))
    return(stage_vector_files(extracted, destination, source = "zip"))
  }

  stage_vector_files(copied, destination, source = "upload")
}

copy_upload_to_named_file <- function(upload, destination) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(destination, basename(upload$name[[1]]))
  if (!file.copy(upload$datapath[[1]], target, overwrite = TRUE)) {
    stop("Não foi possível preparar o arquivo enviado.", call. = FALSE)
  }
  normalizePath(target, mustWork = TRUE)
}

safe_extract_zip <- function(
  zip_path,
  destination,
  max_files = 1000L,
  max_uncompressed_bytes = 2 * 1024^3
) {
  if (!file.exists(zip_path) || tolower(tools::file_ext(zip_path)) != "zip") {
    stop("O arquivo informado não é um ZIP válido.", call. = FALSE)
  }

  entries <- tryCatch(
    utils::unzip(zip_path, list = TRUE),
    error = function(e) {
      stop("Não foi possível ler o ZIP: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (!nrow(entries)) stop("O ZIP está vazio.", call. = FALSE)
  if (nrow(entries) > max_files) {
    stop("O ZIP contém arquivos demais para ser processado.", call. = FALSE)
  }
  if (sum(as.numeric(entries$Length), na.rm = TRUE) > max_uncompressed_bytes) {
    stop("O conteúdo descompactado do ZIP excede o limite permitido.", call. = FALSE)
  }

  entry_names <- gsub("\\\\", "/", entries$Name)
  path_parts <- strsplit(entry_names, "/", fixed = TRUE)
  invalid <- startsWith(entry_names, "/") |
    grepl("^[A-Za-z]:", entry_names) |
    vapply(path_parts, function(x) any(x == ".."), logical(1))
  if (any(invalid)) {
    stop("O ZIP contém caminhos inseguros e não pode ser aberto.", call. = FALSE)
  }

  safe_unlink(destination)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_path, exdir = destination, overwrite = TRUE)

  extracted <- list.files(
    destination,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  extracted <- extracted[file.info(extracted)$isdir %in% FALSE]
  if (!length(extracted)) {
    stop("Nenhum arquivo foi encontrado dentro do ZIP.", call. = FALSE)
  }

  destination_root <- paste0(normalizePath(destination, mustWork = TRUE), .Platform$file.sep)
  normalized <- normalizePath(extracted, mustWork = TRUE)
  if (any(!startsWith(normalized, destination_root))) {
    stop("O ZIP tentou salvar arquivos fora da pasta temporária.", call. = FALSE)
  }
  if (any(nzchar(Sys.readlink(normalized)))) {
    stop("O ZIP contém links simbólicos, que não são permitidos.", call. = FALSE)
  }

  normalized
}

canonical_result_level <- function(source_name) {
  normalize_one <- function(value) {
    value <- iconv(as.character(value %||% ""), from = "", to = "ASCII//TRANSLIT")
    value <- tolower(trimws(value))
    value <- gsub("[^a-z0-9]+", "_", value)
    value <- gsub("^_+|_+$", "", value)

    aliases <- c(
      grid = "grid",
      grid_id = "grid",
      quadrante = "grid",
      quadricula = "grid",
      mesh = "mesh",
      malha = "mesh",
      id_mesh = "mesh",
      groups = "groups",
      group = "groups",
      grupos = "groups",
      grupo = "groups",
      groups_1 = "groups_1",
      group_1 = "groups_1",
      grupos_1 = "groups_1",
      grupo_1 = "groups_1",
      groups_2 = "groups_2",
      group_2 = "groups_2",
      grupos_2 = "groups_2",
      grupo_2 = "groups_2",
      total = "total"
    )
    matched <- unname(aliases[value])
    if (!length(matched) || is.na(matched)) NA_character_ else matched
  }

  vapply(source_name, normalize_one, character(1), USE.NAMES = FALSE)
}

result_level_catalog <- function(files) {
  empty <- data.frame(
    level = character(),
    path = character(),
    source = character(),
    spatial = logical(),
    priority = integer(),
    stringsAsFactors = FALSE
  )
  files <- unique(as.character(files %||% character()))
  files <- files[file.exists(files)]
  files <- files[!startsWith(basename(files), "._")]
  if (!length(files)) return(empty)

  entries <- lapply(files, function(path) {
    extension <- tolower(tools::file_ext(path))
    sources <- character()
    spatial <- FALSE
    priority <- 0L

    if (identical(extension, "gpkg")) {
      sources <- tryCatch(sf::st_layers(path)$name, error = function(e) character())
      spatial <- TRUE
      priority <- 30L
    } else if (identical(extension, "xlsx") && requireNamespace("readxl", quietly = TRUE)) {
      sources <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
      priority <- 20L
    } else if (identical(extension, "rds")) {
      sources <- tryCatch(names(readRDS(path)), error = function(e) character())
      priority <- 10L
    }
    if (!length(sources)) return(NULL)

    levels <- canonical_result_level(sources)
    keep <- !is.na(levels)
    if (!any(keep)) return(NULL)
    data.frame(
      level = levels[keep],
      path = normalizePath(path, mustWork = TRUE),
      source = sources[keep],
      spatial = spatial,
      priority = priority,
      stringsAsFactors = FALSE
    )
  })
  entries <- Filter(Negate(is.null), entries)
  if (!length(entries)) return(empty)

  catalog <- do.call(rbind, entries)
  known_order <- c("grid", "mesh", "groups", "groups_1", "groups_2", "total")
  catalog$order <- match(catalog$level, known_order)
  catalog <- catalog[order(catalog$order, -catalog$priority), , drop = FALSE]
  catalog <- catalog[!duplicated(catalog$level), , drop = FALSE]
  catalog$order <- NULL
  rownames(catalog) <- NULL
  catalog
}

result_measure_columns <- function(data) {
  data <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else as.data.frame(data)
  numeric_columns <- names(data)[vapply(data, is.numeric, logical(1))]
  numeric_columns <- setdiff(numeric_columns, "Year")
  variation_columns <- names(data)[grepl("^(Variation|Growth)_", names(data))]
  base_columns <- sub("^(Variation|Growth)_", "", variation_columns)
  fixed_columns <- c("Deforestation", "Reforestation")
  measures <- intersect(
    unique(c(variation_columns, base_columns, fixed_columns)),
    numeric_columns
  )

  if (!length(measures)) {
    measures <- setdiff(numeric_columns, c("Grid_ID", "ID_mesh"))
  }
  measures
}

result_group_columns <- function(data, level = NULL, known_group_columns = character()) {
  data <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else as.data.frame(data)
  names_available <- names(data)
  level <- canonical_result_level(level %||% "")[[1]]

  if (identical(level, "grid") && "Grid_ID" %in% names_available) return("Grid_ID")
  if (identical(level, "mesh") && "ID_mesh" %in% names_available) return("ID_mesh")
  if (identical(level, "total")) return(character())

  known <- intersect(as.character(known_group_columns %||% character()), names_available)
  if (length(known) && level %in% c("groups", "groups_1", "groups_2")) {
    if (identical(level, "groups_1")) return(known[[1]])
    if (identical(level, "groups_2") && length(known) > 1L) return(known[[2]])
    return(known)
  }

  excluded <- unique(c(
    "Year", "AnalysisLevel", "Grid_ID", "ID_mesh", "BoundaryGroup",
    result_measure_columns(data)
  ))
  candidates <- setdiff(names_available, excluded)
  candidates <- candidates[!grepl("^(Variation|Growth)_", candidates)]

  if (level %in% c("groups", "groups_1", "groups_2")) return(candidates)
  unique(c(intersect(c("Grid_ID", "ID_mesh", "BoundaryGroup"), names_available), candidates))
}

prepare_chart_level_data <- function(data, level, known_group_columns = character()) {
  table <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else as.data.frame(data)
  level <- canonical_result_level(level)[[1]]
  grouping <- result_group_columns(table, level, known_group_columns)

  if (length(grouping) > 1L) {
    labels <- lapply(grouping, function(column) {
      values <- as.character(table[[column]])
      values[is.na(values) | !nzchar(trimws(values))] <- "Sem valor"
      paste0(column, ": ", values)
    })
    table$.LandScriptChartGroup <- do.call(paste, c(labels, sep = " | "))
    grouping <- ".LandScriptChartGroup"
  } else if (!length(grouping)) {
    grouping <- NULL
  }

  list(data = table, group = grouping)
}

chart_grouping_choices <- function(levels = character(), columns = character()) {
  known_levels <- c("grid", "mesh", "groups", "groups_1", "groups_2", "total")
  level_labels <- c(
    grid = "Grid_ID",
    mesh = "Malha (ID_mesh)",
    groups = "Grupos",
    groups_1 = "Grupos 1",
    groups_2 = "Grupos 2",
    total = "Total"
  )
  levels <- canonical_result_level(levels)
  levels <- known_levels[known_levels %in% levels[!is.na(levels)]]
  columns <- unique(as.character(columns %||% character()))
  columns <- columns[!is.na(columns) & nzchar(columns)]

  c(
    "Sem agrupamento" = "__none__",
    stats::setNames(
      paste0("__level__:", levels),
      paste0("Nível: ", unname(level_labels[levels]))
    ),
    stats::setNames(
      paste0("__column__:", columns),
      paste0("Coluna: ", columns)
    )
  )
}

select_preferred_result_file <- function(files) {
  files <- normalizePath(files[file.exists(files)], mustWork = TRUE)
  extensions <- tolower(tools::file_ext(files))

  geopackages <- files[extensions == "gpkg"]
  if (length(geopackages)) {
    scores <- vapply(geopackages, function(path) {
      name <- tolower(basename(path))
      layers <- tryCatch(sf::st_layers(path)$name, error = function(e) character())
      score <- 0
      if ("grid" %in% layers) score <- score + 1500
      if ("mesh" %in% layers) score <- score + 1000
      if (all(c("grid", "mesh", "groups", "total") %in% layers)) score <- score + 700
      if (all(c("mesh", "groups", "total") %in% layers)) score <- score + 500
      if (!startsWith(name, "simplified_") && !grepl("simplificado", name)) {
        score <- score + 300
      }
      if (!grepl("calcrasterpixels|pixel", name)) score <- score + 50
      score + min(file.info(path)$size / 1e9, 1)
    }, numeric(1))
    selected <- geopackages[[which.max(scores)]]
    return(list(
      path = selected,
      type = "GeoPackage completo",
      layers = sf::st_layers(selected)$name
    ))
  }

  vector_files <- files[extensions %in% c(supported_vector_extensions(), shapefile_extensions())]
  if (length(vector_files)) {
    selected <- select_primary_vector_file(vector_files)
    layers <- tryCatch(sf::st_layers(selected)$name, error = function(e) character())
    return(list(
      path = selected,
      type = "arquivo geoespacial",
      layers = layers,
      spatial = TRUE
    ))
  }

  excel_files <- files[extensions == "xlsx"]
  if (length(excel_files)) {
    selected <- excel_files[[which.max(file.info(excel_files)$size)]]
    layers <- if (requireNamespace("readxl", quietly = TRUE)) {
      tryCatch(readxl::excel_sheets(selected), error = function(e) character())
    } else {
      character()
    }
    return(list(
      path = selected,
      type = "Excel",
      layers = layers,
      spatial = FALSE
    ))
  }

  rds_files <- files[extensions == "rds"]
  if (length(rds_files)) {
    valid <- vapply(rds_files, function(path) {
      tryCatch({
        object <- readRDS(path)
        is.list(object) && all(c("mesh", "groups", "total") %in% names(object))
      }, error = function(e) FALSE)
    }, logical(1))
    if (any(valid)) {
      selected <- rds_files[valid][[1]]
      object <- readRDS(selected)
      layers <- c("grid", "mesh", "groups", "total")
      layers <- layers[layers %in% names(object)]
      return(list(path = selected, type = "objeto R completo", layers = layers))
    }
  }

  tables <- files[extensions %in% c("txt", "tsv", "csv")]
  if (length(tables)) {
    names_lower <- tolower(basename(tables))
    eligible <- !startsWith(names_lower, "simplified_") &
      !grepl("simplificado|calcrasterpixels", names_lower)
    if (any(eligible)) tables <- tables[eligible]
    selected <- tables[[which.max(file.info(tables)$size)]]
    return(list(path = selected, type = "tabela completa", layers = NULL, spatial = FALSE))
  }

  stop(
    "O ZIP não contém um resultado compatível. Inclua o GeoPackage completo, ",
    "um shapefile completo, um Excel ou uma tabela completa.",
    call. = FALSE
  )
}

prepare_result_download_zip <- function(result, destination) {
  if (is.null(result) || is.null(result$files) || !length(result$files)) {
    stop("Nenhum resultado está disponível para download.", call. = FALSE)
  }

  wanted_keys <- c(
    "complete_xlsx",
    intersect("run_summary", names(result$files)),
    names(result$files)[grepl("gpkg", names(result$files))]
  )
  wanted_keys <- unique(wanted_keys[nzchar(wanted_keys)])
  files <- result$files[wanted_keys]
  missing_keys <- wanted_keys[
    is.na(files) |
      !nzchar(files) |
      !file.exists(files)
  ]
  if (length(missing_keys)) {
    stop(
      "Não foi possível encontrar todos os arquivos finais para o ZIP: ",
      paste(missing_keys, collapse = ", "),
      call. = FALSE
    )
  }

  files <- unname(files)
  files <- normalizePath(files, mustWork = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  if (!length(files)) {
    stop("Os arquivos de resultado não foram encontrados para montar o ZIP.", call. = FALSE)
  }

  file_names <- basename(files)
  if (anyDuplicated(file_names)) {
    file_names <- make.unique(file_names, sep = "_")
  }

  source_directories <- unique(dirname(files))
  if (length(source_directories) == 1L && !anyDuplicated(basename(files))) {
    # All final products already live together. Zip them in place instead of
    # making a second full-size copy, which is especially important when disk
    # space is tight.
    zip::zipr(destination, files = basename(files), root = source_directories[[1]])
  } else {
    staging <- tempfile("landscript_results_zip_")
    dir.create(staging, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
    copied <- file.copy(
      from = files,
      to = file.path(staging, file_names),
      overwrite = TRUE,
      copy.mode = FALSE
    )
    if (!all(copied)) {
      failed <- basename(files[!copied])
      stop("Não foi possível copiar para o ZIP: ", paste(failed, collapse = ", "), call. = FALSE)
    }
    zip::zipr(destination, files = file_names, root = staging)
  }

  if (!file.exists(destination) || is.na(file.info(destination)$size) || file.info(destination)$size <= 0) {
    stop("O ZIP de resultados foi criado vazio.", call. = FALSE)
  }

  invisible(destination)
}

safe_unlink <- function(path) {
  if (!is.null(path) && length(path) && any(file.exists(path))) {
    unlink(path, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}

app_alert <- function(text, color = "info", dismissible = FALSE) {
  classes <- paste("alert", paste0("alert-", color), if (dismissible) "alert-dismissible fade show")
  shiny::div(
    class = classes,
    role = "alert",
    text,
    if (dismissible) {
      shiny::tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "alert",
        `aria-label` = "Fechar"
      )
    }
  )
}
