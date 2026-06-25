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
    plot <- plot + ggplot2::facet_wrap(stats::as.formula(paste("~", different.group)), scales = "free_y")
  }
  plot
}

mesh.map <- function(
  mesh.data,
  class = "Deforestation",
  year.used = "all",
  col.limits = c(0, 1, 2, 5),
  col.used = c("white", "#E5E200", "#FC780D", "red", "darkred"),
  grid.color = "transparent",
  classes.column = NULL,
  highlight = NULL,
  title = NULL,
  satellite = FALSE,
  fill.alpha = 1,
  language = "pt-BR"
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
      highlight = "Destaque"
    )
  } else {
    list(
      no = "No",
      to = "to",
      in_year = "in",
      accumulated = "Accumulated",
      between = "from",
      and = "to",
      highlight = "Highlight"
    )
  }
  if (!inherits(mesh.data, "sf")) {
    stop("O mapa requer um objeto geoespacial sf.", call. = FALSE)
  }
  if (!class %in% names(mesh.data)) stop("Classe não encontrada: ", class, call. = FALSE)
  if (!"Year" %in% names(mesh.data)) stop("A tabela precisa conter Year.", call. = FALSE)

  data <- mesh.data
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

  if (length(grouping)) {
    map_data <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
      dplyr::summarise(.MapValue = sum(.data$.MapValue, na.rm = TRUE), .groups = "drop")
  } else {
    map_data <- data |>
      dplyr::summarise(.MapValue = sum(.data$.MapValue, na.rm = TRUE))
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

  outline <- sf::st_sf(geometry = sf::st_sfc(
    suppressWarnings(sf::st_union(sf::st_geometry(map_data))),
    crs = sf::st_crs(map_data)
  ))

  plot <- ggplot2::ggplot()

  if (isTRUE(satellite)) {
    esri_source <- paste0(
      "https://server.arcgisonline.com/ArcGIS/rest/services/",
      "World_Imagery/MapServer/tile/${z}/${y}/${x}.jpg"
    )
    plot <- plot + ggspatial::annotation_map_tile(
      type = esri_source,
      zoomin = -1,
      progress = "none",
      quiet = TRUE
    )
  }

  plot <- plot +
    ggplot2::geom_sf(
      data = map_data,
      ggplot2::aes(fill = .data$.MapClass),
      color = grid.color,
      linewidth = 0.15,
      alpha = max(0, min(1, fill.alpha))
    ) +
    ggplot2::geom_sf(data = outline, fill = NA, color = "#17212b", linewidth = 0.7) +
    ggplot2::scale_fill_manual(values = col.used, drop = FALSE, na.value = "grey85") +
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
        override.aes = list(colour = "black", linewidth = 0.6, alpha = 1)
      )
    ) +
    ggplot2::theme(
      legend.position = "right",
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
  data <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else data
  names(data)[vapply(data, is.numeric, logical(1)) & names(data) != "Year"]
}
