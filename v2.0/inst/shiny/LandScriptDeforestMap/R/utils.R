`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

app_root <- function() {
  normalizePath(getOption("landscript.app_dir", getwd()), mustWork = FALSE)
}

required_packages <- function() {
  c(
    "shiny", "bslib", "leaflet", "sf", "terra", "dplyr", "tidyr",
    "ggplot2", "ggspatial", "DT", "callr", "processx", "jsonlite",
    "zip", "htmltools", "scales", "shinyFiles", "writexl", "rosm",
    "prettymapr"
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

extract_year_from_name <- function(path) {
  name <- basename(path)
  hits <- regmatches(name, gregexpr("(?<![0-9])(?:19|20)[0-9]{2}(?![0-9])", name, perl = TRUE))[[1]]
  if (!length(hits) || identical(hits, character(0))) return(NA_integer_)
  as.integer(utils::tail(hits, 1L))
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
    ignore.case = TRUE
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  if (!length(files)) {
    stop("Nenhum raster suportado foi encontrado na pasta.", call. = FALSE)
  }

  years <- vapply(files, extract_year_from_name, integer(1))
  if (anyNA(years)) {
    stop(
      "Não foi possível identificar um ano de quatro dígitos no nome de: ",
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

  area_raster <- terra::cellSize(raster, unit = "km")
  center <- data.frame(
    x = mean(c(terra::xmin(raster), terra::xmax(raster))),
    y = mean(c(terra::ymin(raster), terra::ymax(raster)))
  )
  area <- terra::extract(area_raster, center)[[2]]
  if (!length(area) || !is.finite(area) || area <= 0) {
    stop("Não foi possível calcular a área do pixel de ", basename(path), ".", call. = FALSE)
  }
  as.numeric(area)
}

normalize_pixel_area_km2 <- function(area, tolerance = 0.03) {
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

inspect_raster_folder <- function(folder) {
  index <- list_raster_files(folder)
  metadata <- lapply(index$path, function(path) {
    raster <- terra::rast(path)
    if (terra::nlyr(raster) != 1L) {
      stop("O raster deve possuir uma única camada: ", basename(path), call. = FALSE)
    }
    crs <- terra::crs(raster)
    if (!nzchar(crs)) stop("Raster sem CRS: ", basename(path), call. = FALSE)
    list(
      crs = crs,
      crs_label = terra::crs(raster, proj = TRUE),
      resolution = terra::res(raster),
      dimensions = c(terra::ncol(raster), terra::nrow(raster)),
      pixel_area_km2 = raster_pixel_area_km2(path)
    )
  })

  crs_values <- vapply(metadata, `[[`, character(1), "crs")
  resolution_values <- do.call(rbind, lapply(metadata, `[[`, "resolution"))
  pixel_areas <- vapply(metadata, `[[`, numeric(1), "pixel_area_km2")
  normalized_area <- normalize_pixel_area_km2(stats::median(pixel_areas))
  same_crs <- length(unique(crs_values)) == 1L
  same_resolution <- all(apply(
    resolution_values,
    2,
    function(x) isTRUE(all.equal(x, rep(x[[1]], length(x)), tolerance = 1e-8))
  ))

  index$pixel_area_km2 <- pixel_areas
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
    same_crs = same_crs,
    same_resolution = same_resolution
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

write_progress <- function(path, percent, stage, detail = NULL, status = "running") {
  payload <- list(
    percent = max(0, min(100, as.numeric(percent))),
    stage = as.character(stage),
    detail = as.character(detail %||% ""),
    status = status,
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(payload, tmp, auto_unbox = TRUE, pretty = FALSE)
  file.rename(tmp, path)
  invisible(payload)
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

stage_uploaded_vector <- function(upload, destination) {
  if (is.null(upload) || !nrow(upload)) {
    stop("Selecione um arquivo geoespacial.", call. = FALSE)
  }

  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  copied <- file.path(destination, basename(upload$name))
  ok <- file.copy(upload$datapath, copied, overwrite = TRUE)
  if (!all(ok)) stop("Não foi possível preparar os arquivos enviados.", call. = FALSE)

  extensions <- tolower(tools::file_ext(copied))
  priority <- c("gpkg", "geojson", "json", "shp", "kml", "gml")
  primary <- NULL
  for (ext in priority) {
    candidate <- copied[extensions == ext]
    if (length(candidate)) {
      primary <- candidate[[1]]
      break
    }
  }
  if (is.null(primary)) {
    stop(
      "Formato não reconhecido. Envie GeoPackage, GeoJSON ou todos os componentes do shapefile.",
      call. = FALSE
    )
  }

  normalizePath(primary, mustWork = TRUE)
}

copy_upload_to_named_file <- function(upload, destination) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(destination, basename(upload$name[[1]]))
  if (!file.copy(upload$datapath[[1]], target, overwrite = TRUE)) {
    stop("Não foi possível preparar o arquivo enviado.", call. = FALSE)
  }
  normalizePath(target, mustWork = TRUE)
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
