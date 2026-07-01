drop_zm_geometry <- function(geo) {
  if (!inherits(geo, "sf")) return(geo)
  tryCatch(
    sf::st_zm(geo, drop = TRUE, what = "ZM"),
    error = function(e) geo
  )
}

repair_polygon_geometry <- function(geo) {
  if (!inherits(geo, c("sf", "sfc", "sfg"))) return(geo)
  if (inherits(geo, "sfg")) geo <- sf::st_sfc(geo)

  repaired <- tryCatch(
    suppressWarnings(sf::st_make_valid(geo)),
    error = function(e) geo
  )
  repaired <- tryCatch(
    suppressWarnings(sf::st_collection_extract(repaired, "POLYGON")),
    error = function(e) repaired
  )

  if (inherits(repaired, "sf")) {
    repaired <- drop_zm_geometry(repaired)
    empty <- sf::st_is_empty(repaired)
    if (any(empty)) repaired <- repaired[!empty, , drop = FALSE]
    if (nrow(repaired)) sf::st_geometry(repaired) <- "geometry"
  } else if (inherits(repaired, "sfc")) {
    empty <- sf::st_is_empty(repaired)
    if (any(empty)) repaired <- repaired[!empty]
  }

  repaired
}

safe_st_union <- function(x, y = NULL) {
  result <- if (is.null(y)) {
    suppressMessages(suppressWarnings(sf::st_union(x)))
  } else {
    suppressMessages(suppressWarnings(sf::st_union(x, y)))
  }
  repair_polygon_geometry(result)
}

invalid_geometry_message <- function() {
  "arquivo com geometria inválida, favor corrigir ele e tentar novamente"
}

validate_shapefile_geometry <- function(file_name) {
  if (is.null(file_name) || !length(file_name) || !file.exists(file_name[[1]])) {
    return(invisible(TRUE))
  }
  file_name <- normalizePath(file_name[[1]], mustWork = TRUE)
  if (!identical(tolower(tools::file_ext(file_name)), "shp")) {
    return(invisible(TRUE))
  }

  geo <- sf::st_read(file_name, quiet = TRUE, stringsAsFactors = FALSE)
  if (!inherits(geo, "sf") || !nrow(geo)) {
    return(invisible(TRUE))
  }

  valid_geometry <- suppressWarnings(sf::st_is_valid(geo))
  if (any(!valid_geometry | is.na(valid_geometry))) {
    stop(invalid_geometry_message(), call. = FALSE)
  }

  invisible(TRUE)
}

read_geo <- function(file_name, projection_wanted = 4326, layer = NULL) {
  if (inherits(file_name, "sf")) {
    geo <- file_name
  } else {
    if (!file.exists(file_name)) stop("Arquivo geoespacial não encontrado.", call. = FALSE)
    geo <- if (is.null(layer) || !nzchar(layer)) {
      sf::st_read(file_name, quiet = TRUE, stringsAsFactors = FALSE)
    } else {
      sf::st_read(file_name, layer = layer, quiet = TRUE, stringsAsFactors = FALSE)
    }
  }

  if (!inherits(geo, "sf") || !nrow(geo)) {
    stop("O arquivo geoespacial não contém feições.", call. = FALSE)
  }
  geo <- drop_zm_geometry(geo)
  if (is.na(sf::st_crs(geo))) {
    stop("O arquivo geoespacial não possui sistema de coordenadas (CRS).", call. = FALSE)
  }

  valid_geometry <- suppressWarnings(sf::st_is_valid(geo))
  if (any(!valid_geometry | is.na(valid_geometry))) {
    geo <- suppressWarnings(sf::st_make_valid(geo))
  }
  empty <- sf::st_is_empty(geo)
  if (any(empty)) geo <- geo[!empty, , drop = FALSE]
  if (!nrow(geo)) stop("Todas as geometrias do arquivo estão vazias.", call. = FALSE)

  geometry_types <- unique(as.character(sf::st_geometry_type(geo, by_geometry = TRUE)))
  if (!all(grepl("POLYGON", geometry_types))) {
    stop(
      "A análise requer geometrias poligonais. Tipos encontrados: ",
      paste(geometry_types, collapse = ", "),
      call. = FALSE
    )
  }

  geo <- suppressWarnings(sf::st_collection_extract(geo, "POLYGON"))
  geo <- sf::st_transform(geo, projection_wanted)
  geo <- drop_zm_geometry(geo)
  sf::st_geometry(geo) <- "geometry"
  geo
}

geo_summary <- function(geo) {
  bbox <- sf::st_bbox(geo)
  data.frame(
    item = c("Feições", "CRS", "Geometria", "Colunas de atributos", "Extensão"),
    value = c(
      format(nrow(geo), big.mark = ".", decimal.mark = ","),
      sf::st_crs(geo)$input %||% paste0("EPSG:", sf::st_crs(geo)$epsg),
      paste(unique(as.character(sf::st_geometry_type(geo))), collapse = ", "),
      length(sf::st_drop_geometry(geo)),
      paste(round(bbox, 5), collapse = " | ")
    ),
    stringsAsFactors = FALSE
  )
}

local_metric_crs <- function(geo) {
  geo_wgs84 <- sf::st_transform(geo, 4326)
  center <- suppressWarnings(sf::st_point_on_surface(safe_st_union(geo_wgs84)))
  coordinates <- sf::st_coordinates(center)[1, ]
  longitude <- coordinates[["X"]]
  latitude <- coordinates[["Y"]]

  if (is.finite(latitude) && latitude >= -80 && latitude <= 84) {
    zone <- max(1L, min(60L, floor((longitude + 180) / 6) + 1L))
    epsg <- if (latitude >= 0) 32600L + zone else 32700L + zone
    return(sf::st_crs(epsg))
  }

  sf::st_crs(paste0(
    "+proj=aeqd +lat_0=", latitude,
    " +lon_0=", longitude,
    " +datum=WGS84 +units=m +no_defs"
  ))
}

meters_to_degree_equivalent <- function(meters, geo = NULL) {
  if (is.null(meters) || length(meters) != 1L || !is.finite(meters) || meters <= 0) {
    return(list(meters = NA_real_, latitude_degrees = NA_real_, longitude_degrees = NA_real_, latitude = NA_real_))
  }

  latitude <- 0
  if (!is.null(geo) && inherits(geo, "sf") && nrow(geo)) {
    center <- suppressWarnings(sf::st_point_on_surface(safe_st_union(sf::st_transform(geo, 4326))))
    latitude <- unname(sf::st_coordinates(center)[1, "Y"])
  }
  latitude_radians <- latitude * pi / 180
  meters_per_degree_latitude <-
    111132.92 -
    559.82 * cos(2 * latitude_radians) +
    1.175 * cos(4 * latitude_radians) -
    0.0023 * cos(6 * latitude_radians)
  meters_per_degree_longitude <-
    111412.84 * cos(latitude_radians) -
    93.5 * cos(3 * latitude_radians) +
    0.118 * cos(5 * latitude_radians)

  list(
    meters = meters,
    latitude_degrees = unname(meters / meters_per_degree_latitude),
    longitude_degrees = unname(meters / meters_per_degree_longitude),
    latitude = latitude
  )
}

recommend_mesh_size_km <- function(
  geo,
  default_km = 5,
  max_cells = 20000L,
  step_km = 5,
  group_column = NULL,
  verify_actual = !is.null(group_column) && nzchar(group_column)
) {
  default_km <- suppressWarnings(as.numeric(default_km %||% 5))
  if (!is.finite(default_km) || default_km <= 0) default_km <- 5
  max_cells <- as.numeric(max_cells %||% 20000L)
  if (!is.finite(max_cells) || max_cells <= 0) max_cells <- 20000L
  step_km <- suppressWarnings(as.numeric(step_km %||% 5))
  if (!is.finite(step_km) || step_km <= 0) step_km <- 5

  geo <- read_geo(geo)
  metric_geo <- sf::st_transform(geo, local_metric_crs(geo))
  bbox <- sf::st_bbox(metric_geo)
  width <- as.numeric(bbox[["xmax"]] - bbox[["xmin"]])
  height <- as.numeric(bbox[["ymax"]] - bbox[["ymin"]])

  if (!is.finite(width) || !is.finite(height) || width <= 0 || height <= 0) {
    return(list(
      size_km = default_km,
      estimated_cells = NA_real_,
      adjusted = FALSE,
      max_cells = max_cells
    ))
  }

  estimate_count <- function(size_km) {
    size_m <- size_km * 1000
    as.numeric(max(1, ceiling(width / size_m)) * max(1, ceiling(height / size_m)))
  }

  actual_count <- function(size_km) {
    if (!isTRUE(verify_actual)) return(estimate_count(size_km))
    tryCatch(
      nrow(create.mesh(
        geo,
        mesh.size = size_km * 1000,
        group.column = group_column,
        max.cells = max_cells,
        mesh.unit = "meters"
      )),
      error = function(e) Inf
    )
  }

  verified_count <- function(size_km) {
    estimated <- estimate_count(size_km)
    if (!is.finite(estimated) || estimated > max_cells) return(Inf)
    actual_count(size_km)
  }

  default_count <- estimate_count(default_km)
  default_actual_count <- verified_count(default_km)
  if (is.finite(default_count) && default_count <= max_cells &&
      is.finite(default_actual_count) && default_actual_count <= max_cells) {
    return(list(
      size_km = default_km,
      estimated_cells = default_actual_count,
      adjusted = FALSE,
      max_cells = max_cells,
      previous_size_km = default_km,
      previous_estimated_cells = default_actual_count
    ))
  }

  approximate_km <- sqrt((width * height) / max_cells) / 1000
  recommended_km <- ceiling(max(default_km, approximate_km) / step_km) * step_km
  if (!is.finite(recommended_km) || recommended_km <= 0) recommended_km <- step_km

  recommended_count <- verified_count(recommended_km)
  guard <- 0L
  while ((!is.finite(recommended_count) || recommended_count > max_cells) && guard < 10000L) {
    recommended_km <- recommended_km + step_km
    recommended_count <- verified_count(recommended_km)
    guard <- guard + 1L
  }
  if (!is.finite(recommended_count)) {
    recommended_count <- estimate_count(recommended_km)
  }

  list(
    size_km = recommended_km,
    estimated_cells = recommended_count,
    adjusted = TRUE,
    max_cells = max_cells,
    previous_size_km = default_km,
    previous_estimated_cells = default_actual_count
  )
}

prepare_group_boundaries <- function(geo, group_column = NULL) {
  geo <- read_geo(geo)
  output_crs <- sf::st_crs(geo)
  working_crs <- local_metric_crs(geo)
  working_geo <- repair_polygon_geometry(sf::st_transform(geo, working_crs))

  previous_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(previous_s2)), add = TRUE)

  if (is.null(group_column) || !nzchar(group_column) || identical(group_column, ".ALL")) {
    group_column <- "BoundaryGroup"
    working_geo[[group_column]] <- factor("Área total")
  } else {
    if (!group_column %in% names(sf::st_drop_geometry(working_geo))) {
      stop("A coluna de limites selecionada não existe no arquivo.", call. = FALSE)
    }
    values <- as.character(working_geo[[group_column]])
    values[is.na(values) | !nzchar(trimws(values))] <- "Sem valor"
    working_geo[[group_column]] <- factor(values)
  }

  split_geo <- split(working_geo, as.character(working_geo[[group_column]]), drop = TRUE)
  dissolved <- lapply(names(split_geo), function(level) {
    geometry <- safe_st_union(sf::st_geometry(split_geo[[level]]))
    geometry <- repair_polygon_geometry(sf::st_sfc(geometry, crs = sf::st_crs(working_geo)))
    if (!length(geometry) || all(sf::st_is_empty(geometry))) return(NULL)
    out <- data.frame(value = level, stringsAsFactors = FALSE)
    names(out) <- group_column
    sf::st_sf(out, geometry = geometry)
  })
  dissolved <- Filter(Negate(is.null), dissolved)
  if (!length(dissolved)) {
    stop("Não foi possível criar limites válidos a partir da coluna selecionada.", call. = FALSE)
  }
  boundaries <- do.call(rbind, dissolved)
  rownames(boundaries) <- NULL

  # Remove sobreposições entre grupos de forma determinística. O primeiro nível
  # mantém a área; níveis seguintes recebem apenas a parcela ainda não atribuída.
  if (nrow(boundaries) > 1L) {
    assigned <- NULL
    cleaned <- vector("list", nrow(boundaries))
    overlap_removed <- FALSE

    for (i in seq_len(nrow(boundaries))) {
      current <- repair_polygon_geometry(boundaries[i, , drop = FALSE])
      if (!is.null(assigned)) {
        relation <- suppressMessages(suppressWarnings(sf::st_relate(current, assigned, pattern = "T********")))
        overlap_removed <- overlap_removed || any(lengths(relation) > 0L)
        current <- suppressMessages(suppressWarnings(sf::st_difference(current, assigned)))
        current <- repair_polygon_geometry(current)
      }
      if (nrow(current) && !all(sf::st_is_empty(current))) {
        cleaned[[i]] <- current
        assigned <- if (is.null(assigned)) {
          safe_st_union(current)
        } else {
          safe_st_union(
            assigned,
            safe_st_union(current)
          )
        }
      }
    }
    cleaned <- Filter(Negate(is.null), cleaned)
    boundaries <- do.call(rbind, cleaned)
    attr(boundaries, "overlap_removed") <- overlap_removed
  } else {
    attr(boundaries, "overlap_removed") <- FALSE
  }

  boundaries <- repair_polygon_geometry(boundaries)
  if (!nrow(boundaries)) {
    stop("Não foi possível criar limites válidos a partir da coluna selecionada.", call. = FALSE)
  }
  boundaries <- repair_polygon_geometry(sf::st_transform(boundaries, output_crs))
  if (!nrow(boundaries)) {
    stop("Não foi possível criar limites válidos a partir da coluna selecionada.", call. = FALSE)
  }
  boundaries[[group_column]] <- factor(boundaries[[group_column]])
  sf::st_geometry(boundaries) <- "geometry"
  attr(boundaries, "group_column") <- group_column
  boundaries
}

# Adaptação da função create.mesh v2.0 para preservar grupos e garantir IDs
# únicos após a interseção da grade com os limites.
create.mesh <- function(
  geo.file,
  mesh.size = 0.25,
  mesh.format = "square",
  group.column = NULL,
  max.cells = 50000L,
  mesh.unit = c("degrees", "meters")
) {
  mesh.unit <- match.arg(mesh.unit)
  boundaries <- prepare_group_boundaries(geo.file, group.column)
  group.column <- attr(boundaries, "group_column")

  if (is.null(mesh.size)) {
    mesh <- boundaries
    mesh$Grid_ID <- seq_len(nrow(mesh))
    mesh$ID_mesh <- seq_len(nrow(mesh))
    mesh <- mesh[, c("ID_mesh", "Grid_ID", group.column, "geometry")]
    return(mesh)
  }

  if (!is.numeric(mesh.size) || length(mesh.size) != 1L ||
      is.na(mesh.size) || !is.finite(mesh.size) || mesh.size <= 0) {
    stop("O tamanho da malha deve ser um número positivo.", call. = FALSE)
  }
  if (tolower(mesh.format) != "square") {
    stop("A versão atual suporta apenas malhas quadrangulares.", call. = FALSE)
  }

  original_crs <- sf::st_crs(boundaries)
  working_boundaries <- if (identical(mesh.unit, "meters")) {
    sf::st_transform(boundaries, local_metric_crs(boundaries))
  } else {
    boundaries
  }

  outline <- safe_st_union(working_boundaries)
  grid_geometry <- sf::st_make_grid(outline, cellsize = mesh.size, square = TRUE)
  if (length(grid_geometry) > max.cells) {
    stop(
      "A configuração produziria ",
      format(length(grid_geometry), big.mark = ".", decimal.mark = ","),
      " quadrículas antes do recorte. Aumente o tamanho da malha.",
      call. = FALSE
    )
  }

  grid <- sf::st_sf(
    Grid_ID = seq_along(grid_geometry),
    geometry = grid_geometry
  )
  mesh <- suppressWarnings(sf::st_intersection(grid, working_boundaries))
  mesh <- mesh[!sf::st_is_empty(mesh), , drop = FALSE]
  if (identical(mesh.unit, "meters")) {
    mesh <- sf::st_transform(mesh, original_crs)
  }
  mesh <- drop_zm_geometry(mesh)
  mesh$ID_mesh <- seq_len(nrow(mesh))
  mesh[[group.column]] <- factor(mesh[[group.column]])
  mesh <- mesh[, c("ID_mesh", "Grid_ID", group.column, "geometry")]
  rownames(mesh) <- NULL
  attr(mesh, "group_column") <- group.column
  attr(mesh, "overlap_removed") <- isTRUE(attr(boundaries, "overlap_removed"))
  attr(mesh, "mesh_unit") <- mesh.unit
  attr(mesh, "mesh_size") <- mesh.size
  mesh
}

mesh_for_leaflet <- function(mesh, max_features = 6000L) {
  mesh <- drop_zm_geometry(mesh)
  if (nrow(mesh) <= max_features) return(mesh)
  mesh[seq_len(max_features), , drop = FALSE]
}

restore_numeric_result_column_names <- function(data) {
  if (is.null(names(data))) return(data)

  current_names <- names(data)
  geometry_column <- if (inherits(data, "sf")) attr(data, "sf_column") %||% "" else ""
  protected <- nzchar(geometry_column) & current_names == geometry_column

  proposed_names <- current_names
  raw_class_columns <- grepl("^X[0-9]+$", current_names)
  proposed_names[raw_class_columns] <- sub("^X", "", proposed_names[raw_class_columns])

  variation_columns <- grepl("^Variation_X[0-9]+$", current_names)
  proposed_names[variation_columns] <- sub("^Variation_X", "Variation_", proposed_names[variation_columns])

  proposed_names[protected] <- current_names[protected]
  changed <- proposed_names != current_names & !proposed_names %in% current_names
  current_names[changed] <- proposed_names[changed]

  names(data) <- current_names
  data
}

read_result_dataset <- function(path, layer = NULL) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "gpkg") {
    layers <- sf::st_layers(path)$name
    if (!length(layers)) {
      stop("O GeoPackage não possui camadas.", call. = FALSE)
    }
    selected <- layer %||% if ("mesh" %in% layers) "mesh" else layers[[1]]
    if (!selected %in% layers) {
      stop(
        "A camada '", selected, "' não existe no GeoPackage. Camadas disponíveis: ",
        paste(layers, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    # Não use check.names = FALSE aqui. Em GeoPackages cuja coluna espacial se
    # chama "geom", esse argumento pode impedir o sf de reconhecer a geometria.
    data <- sf::st_read(path, layer = selected, quiet = TRUE)
    if (!inherits(data, "sf") || is.null(attr(data, "sf_column"))) {
      stop("A camada selecionada não possui geometria espacial válida.", call. = FALSE)
    }
    return(restore_numeric_result_column_names(drop_zm_geometry(data)))
  }
  if (extension %in% setdiff(supported_vector_extensions(), "gpkg")) {
    data <- sf::st_read(path, quiet = TRUE)
    if (!inherits(data, "sf") || is.null(attr(data, "sf_column"))) {
      stop("A camada selecionada não possui geometria espacial válida.", call. = FALSE)
    }
    return(restore_numeric_result_column_names(drop_zm_geometry(data)))
  }
  if (extension == "xlsx") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Para ler arquivos .xlsx, instale o pacote readxl.", call. = FALSE)
    }
    sheets <- readxl::excel_sheets(path)
    if (!length(sheets)) {
      stop("O arquivo Excel não possui planilhas.", call. = FALSE)
    }
    selected <- layer %||% if ("Malha" %in% sheets) "Malha" else sheets[[1]]
    if (!selected %in% sheets) selected <- sheets[[1]]
    return(restore_numeric_result_column_names(as.data.frame(readxl::read_excel(path, sheet = selected, .name_repair = "minimal"))))
  }
  if (extension %in% c("csv")) {
    return(restore_numeric_result_column_names(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)))
  }
  if (extension %in% c("txt", "tsv")) {
    return(restore_numeric_result_column_names(utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)))
  }
  if (extension == "rds") {
    object <- readRDS(path)
    if (is.list(object) && all(c("mesh", "groups", "total") %in% names(object))) {
      selected <- layer %||% "mesh"
      return(restore_numeric_result_column_names(object[[selected]]))
    }
    return(restore_numeric_result_column_names(object))
  }
  stop("Formato de resultado não suportado.", call. = FALSE)
}

result_table_data <- function(data) {
  data <- restore_numeric_result_column_names(data)
  if (inherits(data, "sf")) {
    return(sf::st_drop_geometry(data))
  }
  as.data.frame(data)
}
