visuals_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "source-header",
          shiny::span("Fonte dos resultados"),
          shiny::uiOutput(ns("language_buttons"), inline = TRUE)
        )
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        shiny::fileInput(
          ns("manual_result"),
          "Carregar resultados: ZIP, GeoPackage, shapefile ou tabela (opcional)",
          multiple = TRUE,
          accept = c(
            ".zip", ".gpkg", ".shp", ".shx", ".dbf", ".prj", ".cpg", ".qpj",
            ".geojson", ".json", ".kml", ".gml",
            ".xlsx", ".txt", ".tsv", ".csv", ".rds"
          )
        ),
        shiny::selectInput(
          ns("gpkg_layer"),
          "Camada ou planilha",
          choices = c("mesh", "groups", "total"),
          selected = "mesh"
        ),
        shiny::selectInput(
          ns("auto_level"),
          "Resultado automático",
          choices = c("Malha" = "mesh", "Grupos" = "groups", "Total" = "total"),
          selected = "mesh"
        )
      ),
      shiny::uiOutput(ns("source_status")),
      fill = FALSE,
      min_height = "250px",
      class = "source-results-card"
    ),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "Gráfico temporal",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 360,
            shiny::selectInput(ns("primary_column"), "Variável principal", choices = NULL, selected = "Deforestation"),
            shiny::selectizeInput(ns("comparison_columns"), "Variáveis comparadas", choices = NULL, multiple = TRUE),
            shiny::selectInput(ns("chart_group"), "Agrupar por:", choices = c("Sem agrupamento" = "__none__")),
            shiny::textInput(ns("chart_colors"), "Cores das comparações", "purple, grey50, #EA9999, darkorange"),
            shiny::textInput(ns("primary_color"), "Cor da variável principal", "darkgreen"),
            shiny::textInput(ns("chart_title"), "Título", "Desmatamento e variação das classes"),
            shiny::selectInput(ns("chart_format"), "Formato", c("PNG" = "png", "PDF" = "pdf")),
            shiny::numericInput(ns("chart_width"), "Largura (mm)", 254, min = 50, step = 10),
            shiny::numericInput(ns("chart_height"), "Altura (mm)", 140, min = 50, step = 10),
            shiny::downloadButton(ns("download_chart"), "Baixar gráfico", class = "btn-primary w-100")
          ),
          bslib::card(
            full_screen = TRUE,
            shiny::plotOutput(ns("timeseries_plot"), height = "620px")
          )
        )
      ),
      bslib::nav_panel(
        "Mapa dos resultados",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 360,
            shiny::checkboxInput(ns("map_satellite"), "Mostrar fundo de satélite (Esri)", TRUE),
            shiny::conditionalPanel(
              condition = sprintf("input['%s']", ns("map_satellite")),
              shiny::sliderInput(ns("map_fill_alpha"), "Opacidade das classes", min = 0.1, max = 1, value = 0.65, step = 0.05)
            ),
            shiny::selectInput(ns("map_class"), "Classe analisada", choices = NULL, selected = "Deforestation"),
            shiny::selectizeInput(ns("map_years"), "Ano(s)", choices = NULL, multiple = TRUE),
            shiny::textInput(ns("map_limits"), "Limites das classes", "1, 2, 5"),
            shiny::textInput(ns("map_colors"), "Paleta", "white, #E5E200, #FC780D, red, darkred"),
            shiny::selectInput(ns("map_group"), "Coluna de agrupamento", choices = c("Nenhuma" = "")),
            shiny::selectInput(ns("highlight"), "Classe a destacar", choices = c("Nenhuma" = "")),
            shiny::textInput(ns("map_title"), "Título (opcional)", ""),
            shiny::selectInput(ns("map_format"), "Formato", c("PNG" = "png", "PDF" = "pdf")),
            shiny::numericInput(ns("map_width"), "Largura (mm)", 230, min = 50, step = 10),
            shiny::numericInput(ns("map_height"), "Altura (mm)", 180, min = 50, step = 10),
            shiny::downloadButton(ns("download_map"), "Baixar mapa", class = "btn-primary w-100")
          ),
          bslib::card(
            full_screen = TRUE,
            shiny::plotOutput(ns("result_map"), height = "680px")
          )
        )
      )
    )
  )
}

visuals_server <- function(id, automatic_result) {
  shiny::moduleServer(id, function(input, output, session) {
    manual_path <- shiny::reactiveVal(NULL)
    manual_layers <- shiny::reactiveVal(NULL)
    manual_error <- shiny::reactiveVal(NULL)
    manual_source <- shiny::reactiveVal(NULL)
    manual_kind <- shiny::reactiveVal(NULL)
    plot_language <- shiny::reactiveVal("pt-BR")
    upload_dir <- tempfile("landscript_visual_")
    dir.create(upload_dir, recursive = TRUE)

    output$language_buttons <- shiny::renderUI({
      language <- plot_language()
      shiny::div(
        class = "plot-language-switch",
        shiny::actionButton(
          session$ns("language_pt"),
          "🇧🇷 Português (Brasil)",
          class = paste(
            "btn-sm language-button",
            if (language == "pt-BR") "btn-primary active" else "btn-outline-secondary"
          ),
          title = "Traduzir gráfico e mapa para português do Brasil"
        ),
        shiny::actionButton(
          session$ns("language_en"),
          "🇺🇸 English",
          class = paste(
            "btn-sm language-button",
            if (language == "en") "btn-primary active" else "btn-outline-secondary"
          ),
          title = "Translate chart and map into English"
        )
      )
    })

    shiny::observeEvent(input$language_pt, {
      plot_language("pt-BR")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$language_en, {
      plot_language("en")
    }, ignoreInit = TRUE)

    default_chart_titles <- c(
      `pt-BR` = "Desmatamento e variação das classes",
      en = "Deforestation and class variation"
    )
    shiny::observeEvent(plot_language(), {
      current_title <- trimws(input$chart_title %||% "")
      if (!nzchar(current_title) || current_title %in% unname(default_chart_titles)) {
        shiny::updateTextInput(
          session,
          "chart_title",
          value = unname(default_chart_titles[[plot_language()]])
        )
      }
    }, ignoreInit = FALSE)

    chart_title_value <- shiny::reactive({
      current_title <- trimws(input$chart_title %||% "")
      if (!nzchar(current_title) || current_title %in% unname(default_chart_titles)) {
        return(unname(default_chart_titles[[plot_language()]]))
      }
      current_title
    })

    shiny::observeEvent(input$manual_result, {
      tryCatch({
        upload <- input$manual_result
        if (is.null(upload) || !nrow(upload)) return(NULL)
        manual_error(NULL)
        manual_kind(NULL)

        source <- list(
          uploaded = paste(upload$name, collapse = ", "),
          selected = paste(upload$name, collapse = ", "),
          selected_type = NULL,
          from_zip = FALSE
        )

        extensions <- tolower(tools::file_ext(upload$name))
        selected_path <- NULL
        extension <- NULL

        if (length(upload$name) == 1L && extensions[[1]] == "zip") {
          path <- copy_upload_to_named_file(upload, upload_dir)
          extract_dir <- tempfile("landscript_results_", tmpdir = upload_dir)
          extracted <- safe_extract_zip(path, extract_dir)
          selected <- select_preferred_result_file(extracted)
          selected_path <- selected$path
          extension <- tolower(tools::file_ext(selected_path))
          source <- list(
            uploaded = basename(path),
            selected = basename(selected_path),
            selected_type = selected$type,
            from_zip = TRUE
          )
        } else if (any(extensions %in% c(supported_vector_extensions(), shapefile_extensions()))) {
          vector_dir <- file.path(upload_dir, paste0("vector_", as.integer(Sys.time())))
          selected_path <- stage_uploaded_vector(upload, vector_dir)
          extension <- tolower(tools::file_ext(selected_path))
          source <- list(
            uploaded = paste(upload$name, collapse = ", "),
            selected = basename(selected_path),
            selected_type = "arquivo geoespacial",
            from_zip = FALSE
          )
        } else {
          path <- copy_upload_to_named_file(upload, upload_dir)
          selected_path <- path
          extension <- tolower(tools::file_ext(path))
        }

        if (extension == "gpkg") {
          layers <- sf::st_layers(selected_path)$name
          if (!length(layers)) stop("O GeoPackage selecionado não possui camadas.", call. = FALSE)
          manual_layers(layers)
          shiny::updateSelectInput(session, "gpkg_layer", choices = layers, selected = if ("mesh" %in% layers) "mesh" else layers[[1]])
          manual_kind("spatial")
        } else if (extension %in% setdiff(supported_vector_extensions(), "gpkg")) {
          manual_layers(NULL)
          shiny::updateSelectInput(session, "gpkg_layer", choices = c("Camada única" = ""), selected = "")
          manual_kind("spatial")
        } else if (extension == "xlsx") {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            stop("Para ler arquivos .xlsx, instale o pacote readxl.", call. = FALSE)
          }
          sheets <- readxl::excel_sheets(selected_path)
          if (!length(sheets)) stop("O arquivo Excel não possui planilhas.", call. = FALSE)
          manual_layers(sheets)
          shiny::updateSelectInput(session, "gpkg_layer", choices = sheets, selected = if ("Malha" %in% sheets) "Malha" else sheets[[1]])
          manual_kind("table")
        } else if (extension %in% c("txt", "tsv", "csv")) {
          manual_layers(NULL)
          shiny::updateSelectInput(session, "gpkg_layer", choices = c("Tabela" = ""), selected = "")
          manual_kind("table")
        } else if (extension == "rds") {
          object <- readRDS(selected_path)
          layers <- if (is.list(object) && all(c("mesh", "groups", "total") %in% names(object))) {
            c("mesh", "groups", "total")
          } else {
            character()
          }
          manual_layers(layers)
          if (length(layers)) {
            shiny::updateSelectInput(session, "gpkg_layer", choices = layers, selected = "mesh")
          }
          manual_kind("table")
        } else {
          manual_layers(NULL)
          manual_kind("table")
        }

        manual_path(selected_path)
        manual_source(source)
      }, error = function(e) {
        manual_error(conditionMessage(e))
        manual_path(NULL)
        manual_source(NULL)
        manual_kind(NULL)
      })
    })

    current_data <- shiny::reactive({
      if (!is.null(manual_path())) {
        return(read_result_dataset(manual_path(), layer = input$gpkg_layer))
      }
      result <- automatic_result()
      shiny::req(result)
      result[[input$auto_level %||% "mesh"]]
    })

    current_table <- shiny::reactive({
      result_table_data(current_data())
    })

    output$source_status <- shiny::renderUI({
      if (!is.null(manual_error())) {
        return(app_alert(manual_error(), color = "danger"))
      }
      if (!is.null(manual_path())) {
        status <- tryCatch({
          data <- current_data()
          source <- manual_source()
          detail <- if (inherits(data, "sf")) {
            layer_label <- input$gpkg_layer %||% ""
            paste0(
              if (nzchar(layer_label)) paste0(" — camada ", layer_label, ", ") else " — ",
              format(nrow(data), big.mark = ".", decimal.mark = ","),
              " registros com geometria"
            )
          } else {
            paste0(
              " — ",
              format(nrow(data), big.mark = ".", decimal.mark = ","),
              " registros"
            )
          }
          origin <- if (isTRUE(source$from_zip)) {
            paste0(
              "ZIP ", source$uploaded, ": selecionado automaticamente ",
              source$selected_type, " (", source$selected, ")"
            )
          } else {
            paste0("Usando arquivo manual: ", source$selected %||% basename(manual_path()))
          }
          if (!inherits(data, "sf")) {
            return(app_alert(
              paste0(origin, detail, ". Para criação de mapas, é necessário arquivo geoespacial."),
              color = "warning"
            ))
          }
          app_alert(
            paste0(origin, detail),
            color = "info"
          )
        }, error = function(e) {
          app_alert(conditionMessage(e), color = "danger")
        })
        return(status)
      }
      if (!is.null(automatic_result())) {
        return(app_alert("Usando automaticamente o resultado da aba Executar análise.", color = "success"))
      }
      app_alert("Execute uma análise ou carregue um resultado anterior.", color = "secondary")
    })

    shiny::observe({
      data <- tryCatch(current_table(), error = function(e) NULL)
      if (is.null(data)) return()
      numeric_columns <- plot_candidate_columns(data)
      default_primary <- if ("Deforestation" %in% numeric_columns) "Deforestation" else numeric_columns[[1]] %||% ""
      default_comparison <- intersect(
        c("Variation_Agriculture", "Variation_Mining", "Variation_Pasture", "Variation_Urban"),
        numeric_columns
      )
      if (!length(default_comparison)) {
        default_comparison <- utils::head(setdiff(numeric_columns, default_primary), 4)
      }
      shiny::updateSelectInput(session, "primary_column", choices = numeric_columns, selected = default_primary)
      shiny::updateSelectizeInput(session, "comparison_columns", choices = numeric_columns, selected = default_comparison, server = TRUE)

      years <- if ("Year" %in% names(data)) sort(unique(as.integer(as.character(data$Year)))) else integer()

      non_numeric <- names(data)[
        !vapply(data, is.numeric, logical(1))
      ]
      non_numeric <- setdiff(non_numeric, c("AnalysisLevel"))
      current_chart_group <- input$chart_group %||% "__none__"
      chart_group_choices <- c("Sem agrupamento" = "__none__", stats::setNames(non_numeric, non_numeric))
      shiny::updateSelectInput(
        session,
        "chart_group",
        choices = chart_group_choices,
        selected = if (current_chart_group %in% unname(chart_group_choices)) current_chart_group else "__none__"
      )

      spatial_data <- tryCatch(current_data(), error = function(e) NULL)
      if (inherits(spatial_data, "sf")) {
        shiny::updateSelectInput(session, "map_class", choices = numeric_columns, selected = default_primary)
        shiny::updateSelectizeInput(session, "map_years", choices = years, selected = years, server = TRUE)
        shiny::updateSelectInput(session, "map_group", choices = c("Nenhuma" = "", stats::setNames(non_numeric, non_numeric)))
      } else {
        shiny::updateSelectInput(session, "map_class", choices = character(), selected = character())
        shiny::updateSelectizeInput(session, "map_years", choices = character(), selected = character(), server = TRUE)
        shiny::updateSelectInput(session, "map_group", choices = c("Nenhuma" = ""), selected = "")
        shiny::updateSelectInput(session, "highlight", choices = c("Nenhuma" = ""), selected = "")
      }
    })

    shiny::observe({
      data <- tryCatch(current_data(), error = function(e) NULL)
      group <- input$map_group
      if (is.null(data) || is.null(group) || !nzchar(group) || !group %in% names(data)) {
        shiny::updateSelectInput(session, "highlight", choices = c("Nenhuma" = ""))
        return()
      }
      values <- sort(unique(as.character(data[[group]])))
      shiny::updateSelectInput(session, "highlight", choices = c("Nenhuma" = "", stats::setNames(values, values)))
    })

    chart_object <- shiny::reactive({
      data <- current_table()
      chart_group <- input$chart_group %||% "__none__"
      if (identical(chart_group, "__none__")) chart_group <- NULL
      build_timeseries_plot(
        data,
        comparison.columns = input$comparison_columns,
        primary.column = input$primary_column,
        comparison.colors = parse_color_vector(input$chart_colors),
        primary.color = parse_color_vector(input$primary_color)[[1]],
        title.name = chart_title_value(),
        different.group = chart_group,
        language = plot_language()
      )
    })

    output$timeseries_plot <- shiny::renderPlot({
      tryCatch(
        chart_object(),
        error = function(e) {
          graphics::plot.new()
          graphics::text(0.5, 0.5, conditionMessage(e), cex = 1.1)
        }
      )
    }, res = 110)

    map_object <- shiny::reactive({
      data <- current_data()
      if (!inherits(data, "sf")) {
        return(NULL)
      }
      mesh.map(
        data,
        class = input$map_class,
        year.used = input$map_years %||% "all",
        col.limits = parse_number_vector(input$map_limits),
        col.used = parse_color_vector(input$map_colors, minimum = 2),
        grid.color = "#17212B66",
        classes.column = input$map_group,
        highlight = input$highlight,
        title = if (nzchar(trimws(input$map_title %||% ""))) input$map_title else NULL,
        satellite = isTRUE(input$map_satellite),
        fill.alpha = if (isTRUE(input$map_satellite)) input$map_fill_alpha %||% 0.65 else 1,
        language = plot_language()
      )
    })

    output$result_map <- shiny::renderPlot({
      tryCatch(
        {
          map <- map_object()
          if (is.null(map)) {
            graphics::plot.new()
            return(invisible(NULL))
          }
          map
        },
        error = function(e) {
          graphics::plot.new()
          graphics::text(0.5, 0.5, conditionMessage(e), cex = 1.1)
        }
      )
    }, res = 110)

    save_plot <- function(plot, file, format, width, height) {
      ggplot2::ggsave(
        filename = file,
        plot = plot,
        device = format,
        width = width,
        height = height,
        units = "mm",
        dpi = 300,
        bg = "white"
      )
    }

    output$download_chart <- shiny::downloadHandler(
      filename = function() {
        prefix <- if (plot_language() == "pt-BR") "serie_temporal" else "time_series"
        paste0(prefix, ".", input$chart_format)
      },
      content = function(file) save_plot(chart_object(), file, input$chart_format, input$chart_width, input$chart_height)
    )
    output$download_map <- shiny::downloadHandler(
      filename = function() {
        prefix <- if (plot_language() == "pt-BR") "mapa_resultados" else "results_map"
        paste0(prefix, ".", input$map_format)
      },
      content = function(file) {
        map <- map_object()
        if (is.null(map)) {
          stop("Para criação de mapas, é necessário arquivo geoespacial.", call. = FALSE)
        }
        save_plot(map, file, input$map_format, input$map_width, input$map_height)
      }
    )

    session$onSessionEnded(function() safe_unlink(upload_dir))
  })
}
