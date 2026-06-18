visuals_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header("Fonte dos resultados"),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        shiny::fileInput(
          ns("manual_result"),
          "Carregar tabela ou GeoPackage (opcional)",
          accept = c(".gpkg", ".txt", ".tsv", ".csv", ".rds")
        ),
        shiny::selectInput(
          ns("gpkg_layer"),
          "Camada do GeoPackage",
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
            shiny::selectInput(ns("chart_group"), "Agrupar/facetar por", choices = c("Sem agrupamento" = "")),
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
            shiny::selectInput(ns("map_class"), "Classe analisada", choices = NULL, selected = "Deforestation"),
            shiny::selectizeInput(ns("map_years"), "Ano(s)", choices = NULL, multiple = TRUE),
            shiny::textInput(ns("map_limits"), "Limites das classes", "1, 2, 5"),
            shiny::textInput(ns("map_colors"), "Paleta", "white, #E5E200, #FC780D, red, darkred"),
            shiny::checkboxInput(ns("map_satellite"), "Mostrar fundo de satélite (Esri)", FALSE),
            shiny::conditionalPanel(
              condition = sprintf("input['%s']", ns("map_satellite")),
              shiny::sliderInput(ns("map_fill_alpha"), "Opacidade das classes", min = 0.1, max = 1, value = 0.65, step = 0.05)
            ),
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
    upload_dir <- tempfile("landscript_visual_")
    dir.create(upload_dir, recursive = TRUE)

    shiny::observeEvent(input$manual_result, {
      tryCatch({
        path <- copy_upload_to_named_file(input$manual_result, upload_dir)
        manual_path(path)
        manual_error(NULL)
        if (tolower(tools::file_ext(path)) == "gpkg") {
          layers <- sf::st_layers(path)$name
          manual_layers(layers)
          shiny::updateSelectInput(session, "gpkg_layer", choices = layers, selected = if ("mesh" %in% layers) "mesh" else layers[[1]])
        } else {
          manual_layers(NULL)
        }
      }, error = function(e) {
        manual_error(conditionMessage(e))
        manual_path(NULL)
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

    output$source_status <- shiny::renderUI({
      if (!is.null(manual_error())) {
        return(app_alert(manual_error(), color = "danger"))
      }
      if (!is.null(manual_path())) {
        return(app_alert(paste("Usando arquivo manual:", basename(manual_path())), color = "info"))
      }
      if (!is.null(automatic_result())) {
        return(app_alert("Usando automaticamente o resultado da aba Executar análise.", color = "success"))
      }
      app_alert("Execute uma análise ou carregue um resultado anterior.", color = "secondary")
    })

    shiny::observe({
      data <- tryCatch(current_data(), error = function(e) NULL)
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
      shiny::updateSelectInput(session, "map_class", choices = numeric_columns, selected = default_primary)

      years <- if ("Year" %in% names(data)) sort(unique(as.integer(as.character(data$Year)))) else integer()
      shiny::updateSelectizeInput(session, "map_years", choices = years, selected = years, server = TRUE)

      plain_data <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else as.data.frame(data)
      non_numeric <- names(plain_data)[
        !vapply(plain_data, is.numeric, logical(1))
      ]
      non_numeric <- setdiff(non_numeric, c("AnalysisLevel"))
      shiny::updateSelectInput(session, "chart_group", choices = c("Sem agrupamento" = "", stats::setNames(non_numeric, non_numeric)))
      shiny::updateSelectInput(session, "map_group", choices = c("Nenhuma" = "", stats::setNames(non_numeric, non_numeric)))
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
      data <- current_data()
      build_timeseries_plot(
        data,
        comparison.columns = input$comparison_columns,
        primary.column = input$primary_column,
        comparison.colors = parse_color_vector(input$chart_colors),
        primary.color = parse_color_vector(input$primary_color)[[1]],
        title.name = input$chart_title,
        different.group = input$chart_group
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
      shiny::validate(shiny::need(inherits(data, "sf"), "Para gerar o mapa, carregue um GeoPackage ou use o resultado automático."))
      mesh.map(
        data,
        class = input$map_class,
        year.used = input$map_years %||% "all",
        col.limits = parse_number_vector(input$map_limits),
        col.used = parse_color_vector(input$map_colors, minimum = 2),
        grid.color = "transparent",
        classes.column = input$map_group,
        highlight = input$highlight,
        title = if (nzchar(trimws(input$map_title %||% ""))) input$map_title else NULL,
        satellite = isTRUE(input$map_satellite),
        fill.alpha = if (isTRUE(input$map_satellite)) input$map_fill_alpha %||% 0.65 else 1
      )
    })

    output$result_map <- shiny::renderPlot({
      tryCatch(
        map_object(),
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
      filename = function() paste0("serie_temporal.", input$chart_format),
      content = function(file) save_plot(chart_object(), file, input$chart_format, input$chart_width, input$chart_height)
    )
    output$download_map <- shiny::downloadHandler(
      filename = function() paste0("mapa_resultados.", input$map_format),
      content = function(file) save_plot(map_object(), file, input$map_format, input$map_width, input$map_height)
    )

    session$onSessionEnded(function() safe_unlink(upload_dir))
  })
}
