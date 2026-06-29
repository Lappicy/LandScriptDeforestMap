drop_zm_geometry <- function(geo) {
  if (!inherits(geo, "sf")) return(geo)
  tryCatch(
    sf::st_zm(geo, drop = TRUE, what = "ZM"),
    error = function(e) geo
  )
}

safe_st_union <- function(x, y = NULL) {
  if (is.null(y)) {
    return(suppressMessages(suppressWarnings(sf::st_union(x))))
  }
  suppressMessages(suppressWarnings(sf::st_union(x, y)))
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

prepare_group_boundaries <- function(geo, group_column = NULL) {
  geo <- read_geo(geo)
  previous_s2 <- sf::sf_use_s2()
  if (isTRUE(sf::st_is_longlat(geo))) {
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(previous_s2)), add = TRUE)
  }

  if (is.null(group_column) || !nzchar(group_column) || identical(group_column, ".ALL")) {
    group_column <- "BoundaryGroup"
    geo[[group_column]] <- factor("Área total")
  } else {
    if (!group_column %in% names(sf::st_drop_geometry(geo))) {
      stop("A coluna de limites selecionada não existe no arquivo.", call. = FALSE)
    }
    values <- as.character(geo[[group_column]])
    values[is.na(values) | !nzchar(trimws(values))] <- "Sem valor"
    geo[[group_column]] <- factor(values)
  }

  split_geo <- split(geo, as.character(geo[[group_column]]), drop = TRUE)
  dissolved <- lapply(names(split_geo), function(level) {
    geometry <- safe_st_union(sf::st_geometry(split_geo[[level]]))
    out <- data.frame(value = level, stringsAsFactors = FALSE)
    names(out) <- group_column
    sf::st_sf(out, geometry = sf::st_sfc(geometry, crs = sf::st_crs(geo)))
  })
  boundaries <- do.call(rbind, dissolved)
  rownames(boundaries) <- NULL

  # Remove sobreposições entre grupos de forma determinística. O primeiro nível
  # mantém a área; níveis seguintes recebem apenas a parcela ainda não atribuída.
  if (nrow(boundaries) > 1L) {
    assigned <- NULL
    cleaned <- vector("list", nrow(boundaries))
    overlap_removed <- FALSE

    for (i in seq_len(nrow(boundaries))) {
      current <- boundaries[i, , drop = FALSE]
      if (!is.null(assigned)) {
        relation <- suppressWarnings(sf::st_relate(current, assigned, pattern = "T********"))
        overlap_removed <- overlap_removed || any(lengths(relation) > 0L)
        current <- suppressMessages(suppressWarnings(sf::st_difference(current, assigned)))
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
    return(drop_zm_geometry(data))
  }
  if (extension %in% setdiff(supported_vector_extensions(), "gpkg")) {
    data <- sf::st_read(path, quiet = TRUE)
    if (!inherits(data, "sf") || is.null(attr(data, "sf_column"))) {
      stop("A camada selecionada não possui geometria espacial válida.", call. = FALSE)
    }
    return(drop_zm_geometry(data))
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
    return(as.data.frame(readxl::read_excel(path, sheet = selected, .name_repair = "minimal")))
  }
  if (extension %in% c("csv")) {
    return(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE))
  }
  if (extension %in% c("txt", "tsv")) {
    return(utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE))
  }
  if (extension == "rds") {
    object <- readRDS(path)
    if (is.list(object) && all(c("mesh", "groups", "total") %in% names(object))) {
      selected <- layer %||% "mesh"
      return(object[[selected]])
    }
    return(object)
  }
  stop("Formato de resultado não suportado.", call. = FALSE)
}

result_table_data <- function(data) {
  if (inherits(data, "sf")) {
    return(sf::st_drop_geometry(data))
  }
  as.data.frame(data)
}
