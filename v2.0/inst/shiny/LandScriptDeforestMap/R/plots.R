normalize_plot_language <- function(language = "pt-BR") {
  language <- tolower(trimws(as.character(language %||% "pt-BR")[[1]]))
  if (language %in% c("en", "en-us", "en-gb", "english")) "en" else "pt-BR"
}

plot_class_label <- function(x, language = "pt-BR") {
  language <- normalize_plot_language(language)
  x <- as.character(x)

  dictionaries <- list(
    `pt-BR` = c(
      Deforestation = "Desmatamento",
      Reforestation = "Reflorestamento",
      Forest = "Floresta",
      NonForest = "Não floresta",
      Water = "Água",
      Others = "Outros",
      Urban = "Área urbana",
      Mining = "Mineração",
      Pasture = "Pastagem",
      Agriculture = "Agricultura",
      CropLivestock = "Agropecuária",
      Fire = "Fogo"
    ),
    en = c(
      Deforestation = "Deforestation",
      Reforestation = "Reforestation",
      Forest = "Forest",
      NonForest = "Non-forest",
      Water = "Water",
      Others = "Others",
      Urban = "Urban area",
      Mining = "Mining",
      Pasture = "Pasture",
      Agriculture = "Agriculture",
      CropLivestock = "Crop and livestock",
      Fire = "Fire"
    )
  )

  translate_one <- function(value) {
    dictionary <- dictionaries[[language]]
    if (value %in% names(dictionary)) return(unname(dictionary[[value]]))

    prefixes <- c("Variation_", "Growth_")
    prefix <- prefixes[startsWith(value, prefixes)][1] %||% ""
    if (nzchar(prefix)) {
      base <- sub(paste0("^", prefix), "", value)
      base_label <- if (base %in% names(dictionary)) {
        unname(dictionary[[base]])
      } else {
        gsub("_", " ", base, fixed = TRUE)
      }
      if (language == "pt-BR") {
        descriptor <- if (prefix == "Variation_") "Variação de " else "Crescimento de "
        return(paste0(descriptor, base_label))
      }
      descriptor <- if (prefix == "Variation_") " variation" else " growth"
      return(paste0(base_label, descriptor))
    }

    gsub("_", " ", value, fixed = TRUE)
  }

  vapply(x, translate_one, character(1), USE.NAMES = FALSE)
}

build_timeseries_plot <- function(
  proxy.table,
  comparison.columns,
  primary.column = "Deforestation",
  comparison.colors = c("purple", "grey50", "#EA9999", "darkorange"),
  primary.color = "darkgreen",
  title.name = NULL,
  different.group = NULL,
  show.correlation = TRUE,
  language = "pt-BR"
) {
  language <- normalize_plot_language(language)
  text <- if (language == "pt-BR") {
    list(
      default_title = "Desmatamento e variação das classes",
      year = "Ano",
      class = "Classe",
      best_correlation = "Maior correlação",
      unavailable_correlation = "Correlação indisponível para os dados selecionados"
    )
  } else {
    list(
      default_title = "Deforestation and class variation",
      year = "Year",
      class = "Class",
      best_correlation = "Highest correlation",
      unavailable_correlation = "Correlation unavailable for the selected data"
    )
  }
  if (is.null(title.name) || !nzchar(trimws(title.name))) {
    title.name <- text$default_title
  }

  data <- if (inherits(proxy.table, "sf")) sf::st_drop_geometry(proxy.table) else as.data.frame(proxy.table)
  if (!"Year" %in% names(data)) stop("A tabela precisa conter a coluna Year.", call. = FALSE)
  if (!primary.column %in% names(data)) stop("Variável principal não encontrada.", call. = FALSE)

  comparison.columns <- intersect(comparison.columns, names(data))
  comparison.columns <- setdiff(comparison.columns, primary.column)
  if (!length(comparison.columns)) {
    stop("Selecione ao menos uma variável de comparação.", call. = FALSE)
  }

  selected <- c(primary.column, comparison.columns)
  data$Year <- as.integer(as.character(data$Year))
  group_columns <- c("Year", if (!is.null(different.group) && nzchar(different.group)) different.group)

  summarized <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_columns))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(selected), ~ sum(as.numeric(.x), na.rm = TRUE)),
      .groups = "drop"
    )

  subtitle <- NULL
  if (isTRUE(show.correlation)) {
    correlation_data <- summarized |>
      dplyr::group_by(.data$Year) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(selected), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
    complete <- stats::na.omit(correlation_data[selected])
    correlations <- suppressWarnings(stats::cor(complete, use = "pairwise.complete.obs"))
    cor_values <- stats::setNames(
      as.numeric(correlations[comparison.columns, primary.column]),
      comparison.columns
    )
    best_name <- names(which.max(abs(cor_values)))
    best_value <- cor_values[[best_name]]
    subtitle <- if (length(best_name) && is.finite(best_value)) {
      paste0(
        text$best_correlation, ": ", plot_class_label(best_name, language), " (",
        if (best_value >= 0) "+" else "", sprintf("%.2f", best_value), ")"
      )
    } else {
      text$unavailable_correlation
    }
  }

  long <- tidyr::pivot_longer(
    summarized,
    cols = dplyr::all_of(selected),
    names_to = "Classe",
    values_to = "Valor"
  )
  long$Classe <- factor(long$Classe, levels = selected)

  comparison.colors <- rep(comparison.colors, length.out = length(comparison.columns))
  palette <- stats::setNames(c(primary.color, comparison.colors), selected)
  display_labels <- plot_class_label(selected, language)
  number_format <- if (language == "pt-BR") {
    function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else {
    function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }

  plot <- ggplot2::ggplot(
    long,
    ggplot2::aes(x = .data$Year, y = .data$Valor, color = .data$Classe)
  ) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.8) +
    ggplot2::geom_point(size = 1.8, alpha = 0.75) +
    ggplot2::scale_color_manual(
      values = palette,
      breaks = selected,
      labels = display_labels
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(
      labels = number_format
    ) +
    ggplot2::labs(
      title = title.name,
      subtitle = subtitle,
      x = text$year,
      y = expression(km^2),
      color = text$class
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (!is.null(different.group) && nzchar(different.group) &&
      different.group %in% names(long)) {
    plot <- plot + ggplot2::facet_wrap(stats::reformulate(different.group), scales = "free_y")
  }
  plot
}

normalize_mesh_dimensions_km <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  value <- value[is.finite(value) & value > 0]
  if (!length(value)) return(NULL)
  if (length(value) == 1L) value <- rep(value, 2L)
  stats::setNames(value[seq_len(2L)], c("width", "height"))
}

infer_mesh_dimensions_km <- function(mesh.data, max_grid_ids = 2000L) {
  if (!inherits(mesh.data, "sf") || !nrow(mesh.data)) return(NULL)

  dimensions <- normalize_mesh_dimensions_km(
    attr(mesh.data, "mesh_dimensions_km", exact = TRUE)
  )
  if (!is.null(dimensions)) return(dimensions)

  size_km <- normalize_mesh_dimensions_km(
    attr(mesh.data, "mesh_size_km", exact = TRUE)
  )
  if (!is.null(size_km)) return(size_km)

  stored_size <- attr(mesh.data, "mesh_size", exact = TRUE)
  stored_unit <- tolower(as.character(
    attr(mesh.data, "mesh_unit", exact = TRUE) %||% ""
  ))
  if (length(stored_size) && length(stored_unit) &&
      is.finite(suppressWarnings(as.numeric(stored_size[[1]])))) {
    stored_size <- as.numeric(stored_size[[1]])
    if (identical(stored_unit[[1]], "meters")) {
      size_km <- normalize_mesh_dimensions_km(stored_size / 1000)
      if (!is.null(size_km)) return(size_km)
    }
  }

  size_columns <- intersect(
    c("mesh_size_km", "Mesh_Size_km", "MeshSizeKm", "Tamanho_Malha_km"),
    names(mesh.data)
  )
  for (column in size_columns) {
    values <- suppressWarnings(as.numeric(mesh.data[[column]]))
    values <- values[is.finite(values) & values > 0]
    if (length(values)) {
      size_km <- normalize_mesh_dimensions_km(stats::median(values))
      if (!is.null(size_km)) return(size_km)
    }
  }

  id_column <- if ("Grid_ID" %in% names(mesh.data)) {
    "Grid_ID"
  } else if ("ID_mesh" %in% names(mesh.data)) {
    "ID_mesh"
  } else {
    NULL
  }
  if (is.null(id_column) || is.na(sf::st_crs(mesh.data))) return(NULL)

  sample_data <- mesh.data
  if ("Year" %in% names(sample_data)) {
    years <- suppressWarnings(as.integer(as.character(sample_data$Year)))
    first_year <- suppressWarnings(min(years, na.rm = TRUE))
    if (is.finite(first_year)) {
      sample_data <- sample_data[years == first_year, , drop = FALSE]
    }
  }
  sample_data <- sample_data[!sf::st_is_empty(sample_data), , drop = FALSE]
  if (!nrow(sample_data)) return(NULL)

  ids <- unique(as.character(sample_data[[id_column]]))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) return(NULL)
  max_grid_ids <- max(1L, as.integer(max_grid_ids))
  if (length(ids) > max_grid_ids) {
    selected <- unique(round(seq(1, length(ids), length.out = max_grid_ids)))
    ids <- ids[selected]
    sample_data <- sample_data[
      as.character(sample_data[[id_column]]) %in% ids,
      ,
      drop = FALSE
    ]
  }

  metric_data <- tryCatch(
    suppressWarnings(sf::st_transform(sample_data, local_metric_crs(sample_data))),
    error = function(e) NULL
  )
  if (is.null(metric_data) || !nrow(metric_data)) return(NULL)

  boxes <- t(vapply(sf::st_geometry(metric_data), function(geometry) {
    bbox <- sf::st_bbox(geometry)
    c(
      xmin = as.numeric(bbox[["xmin"]]),
      ymin = as.numeric(bbox[["ymin"]]),
      xmax = as.numeric(bbox[["xmax"]]),
      ymax = as.numeric(bbox[["ymax"]])
    )
  }, numeric(4)))
  box_data <- data.frame(
    id = as.character(metric_data[[id_column]]),
    boxes,
    stringsAsFactors = FALSE
  )
  box_data <- box_data[stats::complete.cases(box_data), , drop = FALSE]
  if (!nrow(box_data)) return(NULL)

  extents <- lapply(split(box_data, box_data$id), function(rows) {
    c(
      width = max(rows$xmax) - min(rows$xmin),
      height = max(rows$ymax) - min(rows$ymin)
    )
  })
  extents <- do.call(rbind, extents)
  dimensions_m <- as.numeric(extents)
  dimensions_m <- dimensions_m[is.finite(dimensions_m) & dimensions_m > 0]
  if (!length(dimensions_m)) return(NULL)

  # create.mesh() always creates square cells. Border cells can be clipped by
  # the study area, so the largest recovered side is the best estimate of the
  # original, uncut square.
  inferred_size_km <- max(dimensions_m) / 1000
  normalize_mesh_dimensions_km(inferred_size_km)
}

format_mesh_dimension_km <- function(value, language = "pt-BR") {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value) || value <= 0) return(NA_character_)
  digits <- if (abs(value - round(value)) < 0.005) {
    0L
  } else if (value >= 1) {
    1L
  } else {
    2L
  }
  format(
    round(value, digits),
    nsmall = digits,
    trim = TRUE,
    scientific = FALSE,
    decimal.mark = if (identical(language, "pt-BR")) "," else ".",
    big.mark = ""
  )
}

format_mesh_dimensions_km <- function(value, language = "pt-BR") {
  dimensions <- normalize_mesh_dimensions_km(value)
  if (is.null(dimensions)) return(NULL)
  paste0(
    format_mesh_dimension_km(dimensions[["width"]], language),
    " × ",
    format_mesh_dimension_km(dimensions[["height"]], language),
    " km"
  )
}

mesh.map <- function(
  mesh.data,
  class = "Deforestation",
  year.used = "all",
  col.limits = c(0, 1, 2, 5),
  col.used = c("white", "#E5E200", "#FC780D", "red", "darkred"),
  grid.color = "#17212B66",
  classes.column = NULL,
  highlight = NULL,
  title = NULL,
  satellite = FALSE,
  satellite.alpha = 1,
  fill.alpha = 1,
  language = "pt-BR",
  mesh.size.km = NULL
) {
  language <- normalize_plot_language(language)
  text <- if (language == "pt-BR") {
    list(
      no = "Sem",
      to = "a",
      in_year = "em",
      accumulated = "Acumulado de",
      between = "entre",
      and = "e",
      highlight = "Destaque",
      mesh_size = "Tamanho da malha"
    )
  } else {
    list(
      no = "No",
      to = "to",
      in_year = "in",
      accumulated = "Accumulated",
      between = "from",
      and = "to",
      highlight = "Highlight",
      mesh_size = "Mesh size"
    )
  }
  if (!inherits(mesh.data, "sf")) {
    stop("O mapa requer um objeto geoespacial sf.", call. = FALSE)
  }
  if (!class %in% names(mesh.data)) stop("Classe não encontrada: ", class, call. = FALSE)
  if (!"Year" %in% names(mesh.data)) stop("A tabela precisa conter Year.", call. = FALSE)

  data <- mesh.data
  mesh_dimensions_km <- if (is.null(mesh.size.km)) {
    infer_mesh_dimensions_km(data)
  } else {
    normalize_mesh_dimensions_km(mesh.size.km)
  }
  mesh_size_label <- format_mesh_dimensions_km(mesh_dimensions_km, language)
  available_years <- sort(unique(as.integer(as.character(data$Year))))
  if (length(year.used) == 1L && toupper(as.character(year.used)) == "ALL") {
    year.used <- available_years
  } else {
    year.used <- as.integer(year.used)
  }
  data <- data[data$Year %in% year.used, , drop = FALSE]
  if (!nrow(data)) stop("Nenhum dado disponível para os anos selecionados.", call. = FALSE)

  id_candidates <- c("ID_mesh", "Grid_ID")
  id_column <- id_candidates[id_candidates %in% names(data)][1] %||% NULL
  grouping <- c(id_column, if (!is.null(classes.column) && classes.column %in% names(data)) classes.column)
  grouping <- unique(grouping[!is.na(grouping) & nzchar(grouping)])

  values <- as.numeric(data[[class]])
  values[is.na(values) | values < 0] <- 0
  data$.MapValue <- values

  data_table <- sf::st_drop_geometry(data)

  if (length(grouping)) {
    map_values <- data_table |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
      dplyr::summarise(.MapValue = sum(.data$.MapValue, na.rm = TRUE), .groups = "drop")

    geometry_data <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
      dplyr::slice(1L) |>
      dplyr::ungroup()
    geometry_lookup <- sf::st_drop_geometry(geometry_data[, grouping, drop = FALSE])
    geometry_lookup$.geometry_index <- seq_len(nrow(geometry_data))

    map_data <- dplyr::left_join(map_values, geometry_lookup, by = grouping)
    if (anyNA(map_data$.geometry_index)) {
      stop("Não foi possível associar as geometrias aos dados do mapa.", call. = FALSE)
    }
    map_geometry <- sf::st_geometry(geometry_data)[map_data$.geometry_index]
    map_data$.geometry_index <- NULL
    map_data <- sf::st_sf(
      map_data,
      geometry = sf::st_sfc(map_geometry, crs = sf::st_crs(data))
    )
  } else {
    map_geometry <- if (exists("safe_st_union", mode = "function")) {
      safe_st_union(sf::st_geometry(data))
    } else {
      suppressMessages(suppressWarnings(sf::st_union(sf::st_geometry(data))))
    }
    map_data <- sf::st_sf(
      .MapValue = sum(data_table$.MapValue, na.rm = TRUE),
      geometry = sf::st_sfc(map_geometry, crs = sf::st_crs(data))
    )
  }
  if (exists("ensure_valid_polygon_geometry", mode = "function")) {
    map_data <- ensure_valid_polygon_geometry(
      map_data,
      context = "dados do mapa",
      preserve_rows = TRUE
    )
  } else if (exists("repair_polygon_geometry", mode = "function")) {
    map_data <- repair_polygon_geometry(map_data)
  }

  col.limits <- sort(unique(as.numeric(col.limits)))
  col.limits <- col.limits[is.finite(col.limits) & col.limits > 0]
  required_colors <- length(col.limits) + 2L
  col.used <- rep(col.used, length.out = required_colors)
  class_label <- plot_class_label(class, language)
  class_label_lower <- tolower(class_label)

  breaks <- c(-Inf, 0, col.limits, Inf)
  labels <- c(
    paste(text$no, class_label_lower),
    if (length(col.limits)) {
      c(
        paste0("> 0 ", text$to, " ", col.limits[[1]], " km²"),
        if (length(col.limits) > 1L) {
          vapply(seq_len(length(col.limits) - 1L), function(i) {
            paste0(
              "> ", col.limits[[i]], " ", text$to, " ",
              col.limits[[i + 1L]], " km²"
            )
          }, character(1))
        },
        paste0("> ", utils::tail(col.limits, 1L), " km²")
      )
    } else {
      "> 0 km²"
    }
  )
  map_data$.MapClass <- cut(
    map_data$.MapValue,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = TRUE
  )
  map_data$.MapClass <- factor(map_data$.MapClass, levels = labels)
  col.used <- stats::setNames(col.used, labels)
  legend_fills <- unname(col.used[labels])
  legend_seed <- map_data[rep(1L, length(labels)), , drop = FALSE]
  legend_seed$.MapClass <- factor(labels, levels = labels)

  outline_geometry <- if (exists("safe_st_union", mode = "function")) {
    safe_st_union(sf::st_geometry(map_data))
  } else {
    suppressMessages(suppressWarnings(sf::st_union(sf::st_geometry(map_data))))
  }
  outline <- sf::st_sf(geometry = sf::st_sfc(
    outline_geometry,
    crs = sf::st_crs(map_data)
  ))
  if (exists("ensure_valid_polygon_geometry", mode = "function")) {
    outline <- ensure_valid_polygon_geometry(
      outline,
      context = "contorno do mapa",
      preserve_rows = TRUE
    )
  }
  mesh_legend_seed <- NULL
  if (!is.null(mesh_size_label) && nzchar(mesh_size_label)) {
    map_bbox <- sf::st_bbox(map_data)
    mesh_legend_seed <- sf::st_as_sf(
      data.frame(
        .MeshSize = factor(mesh_size_label, levels = mesh_size_label),
        x = mean(c(map_bbox[["xmin"]], map_bbox[["xmax"]])),
        y = mean(c(map_bbox[["ymin"]], map_bbox[["ymax"]]))
      ),
      coords = c("x", "y"),
      crs = sf::st_crs(map_data)
    )
  }

  plot <- ggplot2::ggplot()

  if (isTRUE(satellite)) {
    satellite.alpha <- suppressWarnings(as.numeric(satellite.alpha %||% 1)[[1]])
    if (!is.finite(satellite.alpha)) satellite.alpha <- 1
    satellite.alpha <- max(0, min(1, satellite.alpha))
    esri_source <- paste0(
      "https://server.arcgisonline.com/ArcGIS/rest/services/",
      "World_Imagery/MapServer/tile/${z}/${y}/${x}.jpg"
    )
    plot <- plot + ggspatial::annotation_map_tile(
      type = esri_source,
      zoomin = -1,
      progress = "none",
      quiet = TRUE,
      alpha = satellite.alpha
    )
  }

  plot <- plot +
    ggplot2::geom_sf(
      data = map_data,
      ggplot2::aes(fill = .data$.MapClass),
      color = grid.color,
      linewidth = 0.18,
      alpha = max(0, min(1, fill.alpha))
    ) +
    ggplot2::geom_sf(
      data = legend_seed,
      ggplot2::aes(fill = .data$.MapClass),
      color = NA,
      linewidth = 0,
      alpha = 0,
      show.legend = TRUE
    ) +
    {if (!is.null(mesh_legend_seed)) {
      ggplot2::geom_sf(
        data = mesh_legend_seed,
        ggplot2::aes(shape = .data$.MeshSize),
        color = "transparent",
        fill = "transparent",
        size = 0.01,
        alpha = 0,
        show.legend = c(shape = TRUE, fill = FALSE, colour = FALSE)
      )
    }} +
    ggplot2::geom_sf(data = outline, fill = NA, color = "#17212b", linewidth = 0.7) +
    ggplot2::scale_fill_manual(
      values = col.used,
      limits = labels,
      breaks = labels,
      drop = FALSE,
      na.value = "grey85",
      na.translate = FALSE
    ) +
    {if (!is.null(mesh_legend_seed)) {
      ggplot2::scale_shape_manual(
        name = text$mesh_size,
        values = stats::setNames(22, mesh_size_label),
        limits = mesh_size_label,
        breaks = mesh_size_label,
        drop = FALSE
      )
    }} +
    ggplot2::labs(
      title = title %||% if (length(year.used) == 1L) {
        paste(class_label, text$in_year, year.used)
      } else {
        paste(
          text$accumulated, class_label_lower, text$between,
          min(year.used), text$and, max(year.used)
        )
      },
      subtitle = if (!is.null(highlight) && nzchar(highlight)) {
        paste0(text$highlight, ": ", highlight)
      } else {
        NULL
      },
      x = "Longitude",
      y = "Latitude",
      fill = paste0(class_label, " (km²)")
    ) +
    ggspatial::annotation_scale(location = "br", bar_cols = c("black", "white")) +
    ggspatial::annotation_north_arrow(
      location = "tl",
      which_north = "true",
      height = grid::unit(1, "cm"),
      width = grid::unit(1, "cm"),
      style = ggspatial::north_arrow_orienteering(
        fill = c("black", "white"),
        line_col = "grey20"
      )
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        order = 1,
        override.aes = list(
          fill = legend_fills,
          colour = "black",
          linewidth = 0.6,
          alpha = 1
        )
      ),
      shape = ggplot2::guide_legend(
        order = 2,
        override.aes = list(
          shape = 22,
          fill = "black",
          colour = "black",
          alpha = 1,
          size = 4,
          stroke = 0.6
        )
      )
    ) +
    ggplot2::theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.key = ggplot2::element_rect(colour = "black", linewidth = 0.45),
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.major = ggplot2::element_line(color = "grey90")
    )

  if (!is.null(classes.column) && classes.column %in% names(map_data) &&
      !is.null(highlight) && nzchar(highlight)) {
    highlighted <- map_data[as.character(map_data[[classes.column]]) == highlight, , drop = FALSE]
    if (nrow(highlighted)) {
      plot <- plot + ggplot2::geom_sf(
        data = highlighted,
        fill = NA,
        color = "#00FFFF",
        linewidth = 1.1
      )
    }
  }
  plot
}

plot_candidate_columns <- function(data) {
  if (exists("result_measure_columns", mode = "function")) {
    return(result_measure_columns(data))
  }
  data <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else data
  setdiff(
    names(data)[vapply(data, is.numeric, logical(1))],
    c("Year", "Grid_ID", "ID_mesh")
  )
}
