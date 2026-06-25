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

  raster_pixel_area_from_raster(raster, basename(path))
}

raster_pixel_area_from_raster <- function(raster, label = "raster") {
  area_raster <- terra::cellSize(raster, unit = "km")
  center <- data.frame(
    x = mean(c(terra::xmin(raster), terra::xmax(raster))),
    y = mean(c(terra::ymin(raster), terra::ymax(raster)))
  )
  area <- terra::extract(area_raster, center)[[2]]
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
      paste0(index$year[[i]], " — conferindo CRS, resolução e área do pixel.")
    )

    metadata[[i]] <- list(
      crs = crs,
      crs_label = terra::crs(raster, proj = TRUE),
      resolution = terra::res(raster),
      dimensions = c(terra::ncol(raster), terra::nrow(raster)),
      pixel_area_km2 = raster_pixel_area_from_raster(raster, label)
    )

    report_progress(
      progress,
      segment_end,
      paste0("Validando imagem ", i, " de ", total),
      paste0("Imagem validada: ", index$year[[i]], " — ", basename(path))
    )
  }

  report_progress(progress, 92, "Consolidando validação", "Comparando CRS e resolução entre as imagens.")

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
        "Envie um .zip com todos os componentes ou selecione todos eles juntos."
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
    stop("O ZIP tentou gravar arquivos fora da pasta temporária.", call. = FALSE)
  }
  if (any(nzchar(Sys.readlink(normalized)))) {
    stop("O ZIP contém links simbólicos, que não são permitidos.", call. = FALSE)
  }

  normalized
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
      if ("mesh" %in% layers) score <- score + 1000
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
      return(list(path = selected, type = "objeto R completo", layers = c("mesh", "groups", "total")))
    }
  }

  tables <- files[extensions %in% c("txt", "tsv", "csv")]
  if (length(tables)) {
    names_lower <- tolower(basename(tables))
    eligible <- !startsWith(names_lower, "simplified_") &
      !grepl("simplificado|calcrasterpixels", names_lower)
    if (any(eligible)) tables <- tables[eligible]
    selected <- tables[[which.max(file.info(tables)$size)]]
    return(list(path = selected, type = "tabela completa", layers = NULL))
  }

  stop(
    "O ZIP não contém um resultado compatível. Inclua o GeoPackage completo, ",
    "o arquivo RDS completo ou a tabela completa.",
    call. = FALSE
  )
}

prepare_result_download_zip <- function(result, destination) {
  if (is.null(result) || is.null(result$files) || !length(result$files)) {
    stop("Nenhum resultado está disponível para download.", call. = FALSE)
  }

  files <- unname(result$files[file.exists(result$files)])
  files <- normalizePath(files, mustWork = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  if (!length(files)) {
    stop("Os arquivos de resultado não foram encontrados para montar o ZIP.", call. = FALSE)
  }

  staging <- tempfile("landscript_results_zip_")
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)

  output_root <- normalizePath(result$output_folder %||% dirname(files[[1]]), mustWork = FALSE)
  file_names <- basename(files)
  if (anyDuplicated(file_names)) {
    file_names <- make.unique(file_names, sep = "_")
  }

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

  manifest <- file.path(staging, "README_resultados.txt")
  writeLines(
    c(
      "LandScriptDeforestMap - resultados da análise",
      paste0("Nome da análise: ", result$output_name %||% "LandScript_result"),
      paste0("Pasta original de saída: ", output_root),
      "",
      "Arquivos incluídos:",
      paste0("- ", sort(list.files(staging)))
    ),
    manifest,
    useBytes = TRUE
  )

  zip_files <- list.files(staging, all.files = FALSE, recursive = FALSE)
  zip::zipr(destination, files = zip_files, root = staging)

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
