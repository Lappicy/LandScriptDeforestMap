analysis_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 390,
      open = "desktop",
      bslib::accordion(
        id = ns("steps"),
        open = c("step_geo", "step_mesh"),
        bslib::accordion_panel(
          "1. Arquivo geoespacial",
          value = "step_geo",
          shiny::fileInput(
            ns("geo_upload"),
            "GeoPackage, GeoJSON ou shapefile",
            multiple = TRUE,
            accept = c(".gpkg", ".geojson", ".json", ".shp", ".dbf", ".shx", ".prj", ".cpg")
          ),
          shiny::selectInput(ns("group_column"), "Coluna de limites/grupos", choices = NULL),
          shiny::helpText("As classes serão dissolvidas, recortadas pela malha e analisadas também em conjunto.")
        ),
        bslib::accordion_panel(
          "2. Malha",
          value = "step_mesh",
          shiny::checkboxInput(ns("no_mesh"), "Não criar malha", FALSE),
          shiny::conditionalPanel(
            condition = sprintf("!input['%s']", ns("no_mesh")),
            shiny::numericInput(ns("mesh_size_meters"), "Tamanho da quadrícula (metros)", 5000, min = 1, step = 100),
            shiny::sliderInput(
              ns("mesh_size_slider"),
              "Ajuste por controle",
              min = 100,
              max = 100000,
              value = 5000,
              step = 100,
              post = " m",
              ticks = FALSE
            ),
            shiny::uiOutput(ns("mesh_conversion")),
            shiny::tags$details(
              class = "calculation-details",
              shiny::tags$summary("Como é feita a conversão para graus?"),
              shiny::p(
                "A grade é realmente criada em metros, em uma projeção UTM local. ",
                "Os valores em graus são apenas uma referência aproximada para o mapa."
              ),
              shiny::p(
                "A conversão usa o centro da área de estudo. Um grau de latitude possui ",
                "aproximadamente 111 km; o comprimento de um grau de longitude diminui ",
                "com a latitude, segundo o cosseno da latitude. Por isso os valores de ",
                "longitude e latitude não são exatamente iguais."
              ),
              shiny::p(
                "Em termos simplificados: graus de latitude = metros ÷ metros por grau de ",
                "latitude; graus de longitude = metros ÷ metros por grau de longitude. ",
                "A aplicação usa as fórmulas elipsoidais do WGS 84, com termos de cosseno ",
                "da latitude, em vez de assumir sempre 111.000 m por grau."
              )
            ),
            shiny::helpText("A malha é criada em uma projeção métrica local e reprojetada para exibição no mapa.")
          )
        ),
        bslib::accordion_panel(
          "3. Rasters e classes",
          value = "step_rasters",
          shiny::tags$label(
            class = "control-label",
            `for` = ns("raster_folder_button"),
            "Pasta dos rasters classificados"
          ),
          shiny::div(
            class = "folder-picker-actions",
            shinyFiles::shinyDirButton(
              ns("raster_folder_button"),
              label = "Selecionar pasta…",
              title = "Selecione a pasta que contém os rasters classificados",
              buttonType = "primary",
              class = "btn"
            ),
            shiny::actionButton(
              ns("clear_raster_folder"),
              "Limpar",
              icon = shiny::icon("xmark"),
              class = "btn-outline-secondary"
            )
          ),
          shiny::uiOutput(ns("raster_folder_display")),
          shiny::selectInput(
            ns("mapbiomas"),
            "Legenda MAPBIOMAS",
            choices = c("Coleção 4" = "4", "Coleção 7.1" = "7.1", "Coleção 8" = "8", "Coleção 10" = "10", "Personalizada" = "custom"),
            selected = "8"
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] === 'custom'", ns("mapbiomas")),
            shiny::textInput(ns("class_forest"), "Floresta", "1, 3"),
            shiny::textInput(ns("class_nonforest"), "Não floresta", ""),
            shiny::textInput(ns("class_water"), "Água", "26, 31, 33"),
            shiny::textInput(ns("class_others"), "Outros", "0, 27"),
            shiny::textInput(ns("class_agriculture"), "Agricultura", ""),
            shiny::textInput(ns("class_pasture"), "Pastagem", "15"),
            shiny::textInput(ns("class_mining"), "Mineração", "30"),
            shiny::textInput(ns("class_urban"), "Urbano", "24")
          ),
          shiny::numericInput(ns("pixel_ratio"), "Área de um pixel (km²)", value = 30 * 30 / 1e6, min = 0.0000001, step = 0.000001),
          shiny::uiOutput(ns("pixel_area_info")),
          shiny::tags$details(
            class = "calculation-details",
            shiny::tags$summary("Como a área do pixel é calculada?"),
            shiny::p(
              "A aplicação lê a resolução e o sistema de coordenadas do raster. ",
              "Em rasters projetados, a área vem das dimensões da célula. Em latitude/longitude, ",
              "é calculada a área geodésica da célula central."
            ),
            shiny::p(
              "Quando o resultado fica até 3% de uma resolução usual, ele é normalizado. ",
              "Exemplo: 30 × 30 m = 900 m² = 0,0009 km². ",
              "O campo acima permanece editável para você substituir o valor."
            )
          ),
          shiny::actionButton(
            ns("validate_inputs"),
            "Validar rasters",
            icon = shiny::icon("check-double"),
            class = "btn-outline-primary w-100"
          ),
          shiny::uiOutput(ns("raster_validation_message"))
        ),
        bslib::accordion_panel(
          "4. Saída e execução",
          value = "step_output",
          shiny::textInput(ns("output_folder"), "Pasta de saída", value = file.path(getwd(), "Results")),
          shiny::textInput(ns("output_name"), "Nome da análise", value = "LandScript_result"),
          shiny::actionButton(ns("run"), "Rodar algoritmo", class = "btn-success btn-lg w-100 run-analysis-button", icon = shiny::icon("play")),
          shiny::div(
            class = "run-feedback-box",
            shiny::uiOutput(ns("validation_message")),
            shiny::uiOutput(ns("progress_ui"))
          )
        )
      )
    ),
    bslib::navset_card_tab(
      id = ns("analysis_views"),
      bslib::nav_panel(
        "Mapa e malha",
        leaflet::leafletOutput(ns("preview_map"), height = "650px")
      ),
      bslib::nav_panel(
        "Resumo do arquivo",
        bslib::card(
          bslib::card_header("Informações geoespaciais"),
          shiny::tableOutput(ns("geo_summary"))
        ),
        bslib::card(
          bslib::card_header("Atributos (amostra)"),
          DT::DTOutput(ns("attribute_table"))
        )
      ),
      bslib::nav_panel(
        "Resultados",
        shiny::uiOutput(ns("result_summary")),
        bslib::card(
          bslib::card_header(
            shiny::selectInput(
              ns("result_level"),
              "Nível exibido",
              choices = c("Malha" = "mesh", "Grupos" = "groups", "Total" = "total"),
              selected = "mesh"
            )
          ),
          DT::DTOutput(ns("result_table"))
        ),
        shiny::uiOutput(ns("download_buttons"))
      )
    )
  )
}

analysis_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_dir <- tempfile("landscript_session_")
    dir.create(session_dir, recursive = TRUE)

    geo_path <- shiny::reactiveVal(NULL)
    geo_data <- shiny::reactiveVal(NULL)
    analysis_result <- shiny::reactiveVal(NULL)
    validation_state <- shiny::reactiveVal(NULL)
    raster_validation_state <- shiny::reactiveVal(NULL)
    pixel_area_state <- shiny::reactiveVal(NULL)
    raster_folder_path <- shiny::reactiveVal("")
    running <- shiny::reactiveVal(FALSE)
    job_process <- shiny::reactiveVal(NULL)
    job_progress_file <- shiny::reactiveVal(NULL)
    job_result_file <- shiny::reactiveVal(NULL)
    progress_state <- shiny::reactiveVal(list(percent = 0, stage = "Aguardando", detail = "", status = "idle"))

    directory_roots <- c(
      "Pasta pessoal" = path.expand("~"),
      "Projeto LandScriptDeforestMap" = normalizePath(file.path(app_root(), ".."), mustWork = TRUE),
      shinyFiles::getVolumes()()
    )
    shinyFiles::shinyDirChoose(
      input,
      "raster_folder_button",
      roots = directory_roots,
      session = session,
      defaultRoot = "Pasta pessoal",
      allowDirCreate = FALSE
    )

    shiny::observeEvent(input$raster_folder_button, {
      selected <- shinyFiles::parseDirPath(directory_roots, input$raster_folder_button)
      if (length(selected) && nzchar(selected[[1]])) {
        raster_folder_path(normalizePath(selected[[1]], mustWork = TRUE))
        validation_state(NULL)
        raster_validation_state(NULL)
        tryCatch({
          index <- list_raster_files(selected[[1]])
          raw_pixel_area <- raster_pixel_area_km2(index$path[[1]])
          pixel_area <- normalize_pixel_area_km2(raw_pixel_area)
          shiny::updateNumericInput(session, "pixel_ratio", value = pixel_area$value)
          pixel_area_state(list(
            type = "info",
            text = paste0(
              "Área calculada no primeiro raster: ",
              format(raw_pixel_area, scientific = FALSE, digits = 8), " km². ",
              if (pixel_area$standardized) {
                paste0(
                  "Valor utilizado: ", format(pixel_area$value, scientific = FALSE),
                  " km² (padrão ", pixel_area$standard_label, ")."
                )
              } else {
                paste0("Esse valor foi mantido sem arredondamento.")
              }
            )
          ))
        }, error = function(e) {
          pixel_area_state(list(
            type = "warning",
            text = paste0(
              "Não foi possível detectar automaticamente a área do pixel. ",
              "O valor continua editável. Detalhe: ", conditionMessage(e)
            )
          ))
        })
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$clear_raster_folder, {
      raster_folder_path("")
      validation_state(NULL)
      raster_validation_state(NULL)
      pixel_area_state(NULL)
    })

    output$raster_folder_display <- shiny::renderUI({
      path <- raster_folder_path()
      if (!nzchar(path)) {
        return(shiny::div(
          class = "folder-selection folder-selection-empty",
          shiny::icon("folder-open"),
          shiny::span("Nenhuma pasta selecionada")
        ))
      }
      shiny::div(
        class = "folder-selection",
        shiny::icon("folder"),
        shiny::span(path, title = path)
      )
    })

    shiny::observeEvent(input$geo_upload, {
      tryCatch({
        upload_dir <- file.path(session_dir, paste0("geo_", as.integer(Sys.time())))
        path <- stage_uploaded_vector(input$geo_upload, upload_dir)
        geo <- read_geo(path)
        geo_path(path)
        geo_data(geo)
        columns <- names(sf::st_drop_geometry(geo))
        shiny::updateSelectInput(
          session,
          "group_column",
          choices = c("Toda a área (sem grupos)" = ".ALL", stats::setNames(columns, columns)),
          selected = if (length(columns)) columns[[1]] else ".ALL"
        )
        validation_state(list(type = "success", text = "Arquivo geoespacial carregado com sucesso."))
      }, error = function(e) {
        geo_path(NULL)
        geo_data(NULL)
        validation_state(list(type = "danger", text = conditionMessage(e)))
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$mesh_size_meters, {
      value <- input$mesh_size_meters
      if (!is.null(value) && is.finite(value) && value > 0 &&
          !isTRUE(all.equal(value, input$mesh_size_slider))) {
        shiny::updateSliderInput(
          session,
          "mesh_size_slider",
          min = min(1, value),
          max = max(100000, value),
          value = value
        )
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$mesh_size_slider, {
      value <- input$mesh_size_slider
      if (!is.null(value) && !isTRUE(all.equal(value, input$mesh_size_meters))) {
        shiny::updateNumericInput(session, "mesh_size_meters", value = value)
      }
    }, ignoreInit = TRUE)

    mesh_size_debounced <- shiny::debounce(
      shiny::reactive(input$mesh_size_meters),
      millis = 650
    )

    output$mesh_conversion <- shiny::renderUI({
      value <- input$mesh_size_meters
      if (isTRUE(input$no_mesh) || is.null(value) || !is.finite(value) || value <= 0) return(NULL)
      conversion <- meters_to_degree_equivalent(value, geo_data())
      shiny::div(
        class = "mesh-conversion",
        shiny::icon("ruler-combined"),
        shiny::div(
          shiny::strong(format(round(value, 2), big.mark = ".", decimal.mark = ","), " metros"),
          shiny::br(),
          shiny::span(
            paste0(
              "≈ ", format(conversion$longitude_degrees, digits = 5, decimal.mark = ","),
              "° longitude × ",
              format(conversion$latitude_degrees, digits = 5, decimal.mark = ","),
              "° latitude",
              if (is.finite(conversion$latitude)) {
                paste0(" (na latitude ", format(round(conversion$latitude, 3), decimal.mark = ","), "°)")
              } else ""
            ),
            class = "text-muted small"
          )
        )
      )
    })

    preview_mesh <- shiny::reactive({
      geo <- geo_data()
      shiny::req(geo)
      if (!isTRUE(input$no_mesh)) {
        size <- mesh_size_debounced()
        shiny::validate(shiny::need(!is.null(size) && is.finite(size) && size > 0, "O tamanho da malha deve ser positivo."))
      } else {
        size <- NULL
      }
      create.mesh(
        geo,
        mesh.size = size,
        group.column = input$group_column %||% ".ALL",
        max.cells = 10000L,
        mesh.unit = "meters"
      )
    })

    output$preview_map <- leaflet::renderLeaflet({
      geo <- geo_data()
      map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satélite (Esri)",
          options = leaflet::providerTileOptions(maxZoom = 20)
        ) |>
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "Ruas") |>
        leaflet::hideGroup("Ruas")

      if (is.null(geo)) return(map |> leaflet::setView(lng = -54, lat = -12, zoom = 4))

      mesh <- tryCatch(preview_mesh(), error = function(e) NULL)
      bbox <- sf::st_bbox(geo)
      map <- map |>
        leaflet::addPolygons(
          data = geo,
          fill = FALSE,
          color = "#00E5FF",
          weight = 3,
          opacity = 1,
          group = "Limite original"
        )
      if (!is.null(mesh)) {
        preview <- mesh_for_leaflet(mesh)
        map <- map |>
          leaflet::addPolygons(
            data = preview,
            fillColor = "#F6C344",
            fillOpacity = 0.08,
            color = "#FFD54F",
            weight = 0.7,
            opacity = 0.9,
            group = "Malha"
          )
      }
      map |>
        leaflet::addLayersControl(
          baseGroups = c("Satélite (Esri)", "Ruas"),
          overlayGroups = c("Limite original", "Malha"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    })

    output$geo_summary <- shiny::renderTable({
      shiny::req(geo_data())
      geo_summary(geo_data())
    }, striped = TRUE, bordered = FALSE, spacing = "s")

    output$attribute_table <- DT::renderDT({
      shiny::req(geo_data())
      DT::datatable(
        utils::head(sf::st_drop_geometry(geo_data()), 200),
        rownames = FALSE,
        filter = "top",
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    custom_classes <- shiny::reactive({
      if (!identical(input$mapbiomas, "custom")) return(NULL)
      values <- list(
        Forest = input$class_forest,
        NonForest = input$class_nonforest,
        Water = input$class_water,
        Others = input$class_others,
        Agriculture = input$class_agriculture,
        Pasture = input$class_pasture,
        Mining = input$class_mining,
        Urban = input$class_urban
      )
      lapply(values, parse_number_vector, integer = TRUE)
    })

    output$pixel_area_info <- shiny::renderUI({
      state <- pixel_area_state()
      if (is.null(state)) {
        return(shiny::div(
          class = "field-hint",
          "Esse valor será detectado automaticamente quando uma pasta de rasters for selecionada."
        ))
      }
      app_alert(state$text, color = state$type)
    })

    validate_rasters <- function() {
      inspection <- inspect_raster_folder(raster_folder_path())
      shiny::updateNumericInput(
        session,
        "pixel_ratio",
        value = signif(inspection$pixel_area_km2, 8)
      )
      pixel_area_state(list(
        type = "success",
        text = paste0(
          "Área mediana calculada: ",
          format(inspection$pixel_area_km2_raw, scientific = FALSE, digits = 8),
          " km². Valor utilizado: ",
          format(inspection$pixel_area_km2, scientific = FALSE),
          " km²",
          if (isTRUE(inspection$pixel_area_standardized)) {
            paste0(" (normalizado para ", inspection$pixel_area_standard_label, ").")
          } else {
            "."
          }
        )
      ))
      inspection
    }

    validate_parameters <- function() {
      shiny::req(geo_path(), geo_data())
      if (!isTRUE(input$no_mesh) &&
          (is.null(input$mesh_size_meters) || !is.finite(input$mesh_size_meters) || input$mesh_size_meters <= 0)) {
        stop("O tamanho da malha deve ser um número positivo.", call. = FALSE)
      }
      rasters <- list_raster_files(raster_folder_path())
      output_folder <- ensure_output_folder(input$output_folder)
      if (is.null(input$pixel_ratio) || input$pixel_ratio <= 0) {
        stop("A área por pixel deve ser positiva.", call. = FALSE)
      }
      mapbiomas_class_map(input$mapbiomas, custom_classes())
      list(rasters = rasters, output_folder = output_folder)
    }

    shiny::observeEvent(input$validate_inputs, {
      tryCatch({
        inspection <- shiny::withProgress(
          expr = validate_rasters(),
          message = "Validando rasters",
          detail = "Lendo metadados, CRS, resolução e área dos pixels…",
          value = 0.2
        )
        consistency <- c(
          if (inspection$same_crs) "CRS consistente" else "CRS diferentes",
          if (inspection$same_resolution) "resolução consistente" else "resoluções diferentes"
        )
        raster_validation_state(list(
          type = if (inspection$same_crs && inspection$same_resolution) "success" else "warning",
          text = paste0(
            "Validação concluída: ", inspection$count, " raster(s), anos ",
            inspection$years[[1]], "–", inspection$years[[2]], "; ",
            paste(consistency, collapse = "; "), ". Resolução: ",
            paste(format(inspection$resolution, digits = 7), collapse = " × "), "."
          )
        ))
      }, error = function(e) {
        raster_validation_state(list(type = "danger", text = conditionMessage(e)))
      })
    })

    output$raster_validation_message <- shiny::renderUI({
      message <- raster_validation_state()
      if (is.null(message)) return(NULL)
      shiny::div(
        class = "raster-validation-result",
        app_alert(message$text, color = message$type, dismissible = TRUE)
      )
    })

    output$validation_message <- shiny::renderUI({
      message <- validation_state()
      if (is.null(message)) return(NULL)
      app_alert(message$text, color = message$type, dismissible = TRUE)
    })

    toggle_run_button <- function(disabled) {
      session$sendCustomMessage(
        "toggle-disabled",
        list(id = ns("run"), disabled = isTRUE(disabled))
      )
    }

    shiny::observeEvent(input$run, {
      if (running()) return()
      tryCatch({
        validate_parameters()
        params <- list(
          geo_path = geo_path(),
          group_column = input$group_column,
          no_mesh = isTRUE(input$no_mesh),
          mesh_size = input$mesh_size_meters,
          mesh_unit = "meters",
          raster_folder = normalizePath(raster_folder_path(), mustWork = TRUE),
          output_folder = path.expand(input$output_folder),
          output_name = sanitize_output_name(input$output_name),
          mapbiomas = input$mapbiomas,
          custom_classes = custom_classes(),
          pixel_km2_ratio = input$pixel_ratio,
          max_cells = 50000L
        )

        progress_file <- file.path(session_dir, "progress.json")
        result_file <- file.path(session_dir, "result.rds")
        safe_unlink(c(progress_file, result_file))
        write_progress(progress_file, 0, "Iniciando", "Preparando processo de análise", "starting")

        process <- callr::r_bg(
          func = function(params, progress_file, result_file, app_directory) {
            source(file.path(app_directory, "R", "utils.R"), local = globalenv())
            source(file.path(app_directory, "R", "spatial_io.R"), local = globalenv())
            source(file.path(app_directory, "R", "landscript_engine.R"), local = globalenv())
            run_analysis_job(params, progress_file, result_file, app_directory)
          },
          args = list(
            params = params,
            progress_file = progress_file,
            result_file = result_file,
            app_directory = app_root()
          ),
          supervise = TRUE,
          stdout = "|",
          stderr = "|"
        )
        job_process(process)
        job_progress_file(progress_file)
        job_result_file(result_file)
        running(TRUE)
        toggle_run_button(TRUE)
        analysis_result(NULL)
        bslib::nav_select("analysis_views", "Resultados", session = session)
      }, error = function(e) {
        validation_state(list(type = "danger", text = conditionMessage(e)))
      })
    })

    shiny::observe({
      if (!running()) return()
      shiny::invalidateLater(500, session)
      process <- job_process()
      if (is.null(process)) return()

      progress_state(read_progress(job_progress_file()))
      if (!process$is_alive()) {
        exit_status <- process$get_exit_status()
        running(FALSE)
        toggle_run_button(FALSE)

        if (identical(exit_status, 0L) && file.exists(job_result_file())) {
          result <- readRDS(job_result_file())
          analysis_result(result)
          progress_state(list(
            percent = 100,
            stage = "Concluído",
            detail = "Análise finalizada e arquivos exportados.",
            status = "complete"
          ))
          validation_state(list(type = "success", text = "Análise concluída com sucesso."))
          shiny::showNotification("Análise concluída com sucesso.", type = "message", duration = 8)
        } else {
          error_lines <- c(process$read_error_lines(), process$read_output_lines())
          progress <- read_progress(job_progress_file())
          detail <- progress$detail %||% utils::tail(error_lines[nzchar(error_lines)], 1L) %||% "Falha desconhecida."
          progress_state(list(percent = 100, stage = "Erro", detail = detail, status = "error"))
          validation_state(list(type = "danger", text = detail))
          shiny::showNotification(detail, type = "error", duration = NULL)
        }
      }
    })

    output$progress_ui <- shiny::renderUI({
      state <- progress_state()
      if (identical(state$status, "idle")) return(NULL)
      color <- if (identical(state$status, "error")) "danger" else if (identical(state$status, "complete")) "success" else "primary"
      shiny::tagList(
        shiny::div(
          class = "progress-status",
          shiny::strong(
            if (running()) shiny::icon("gear", class = "fa-spin"),
            paste0(" ", round(state$percent), "% — ", state$stage)
          ),
          shiny::span(state$detail, class = "text-muted small")
        ),
        shiny::div(
          class = "progress",
          role = "progressbar",
          `aria-valuenow` = state$percent,
          `aria-valuemin` = "0",
          `aria-valuemax` = "100",
          shiny::div(
            class = paste("progress-bar progress-bar-striped", if (running()) "progress-bar-animated", paste0("bg-", color)),
            style = paste0("width:", state$percent, "%")
          )
        )
      )
    })

    selected_result_data <- shiny::reactive({
      result <- analysis_result()
      shiny::req(result)
      result[[input$result_level %||% "mesh"]]
    })

    output$result_summary <- shiny::renderUI({
      result <- analysis_result()
      shiny::req(result)
      shiny::tagList(
        if (isTRUE(result$overlap_removed)) {
          app_alert(
            "Foram detectadas sobreposições entre grupos. Cada área sobreposta foi atribuída a apenas um grupo para evitar dupla contagem.",
            color = "warning"
          )
        },
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          bslib::value_box("Unidades da malha", format(nrow(result$mesh), big.mark = "."), showcase = shiny::icon("border-all"), theme = "primary"),
          bslib::value_box("Grupos", format(length(unique(result$groups[[result$group_column]])), big.mark = "."), showcase = shiny::icon("layer-group"), theme = "info"),
          bslib::value_box("Anos", paste0(min(result$raster_index$year), "–", max(result$raster_index$year)), showcase = shiny::icon("calendar"), theme = "success"),
          bslib::value_box("Rasters", nrow(result$raster_index), showcase = shiny::icon("images"), theme = "warning")
        )
      )
    })

    output$result_table <- DT::renderDT({
      data <- selected_result_data()
      DT::datatable(
        sf::st_drop_geometry(data),
        rownames = FALSE,
        filter = "top",
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", text = "Copiar"),
            list(extend = "csv", text = "CSV"),
            list(extend = "excel", text = "Excel")
          ),
          language = list(
            search = "Pesquisar:",
            lengthMenu = "Mostrar _MENU_ linhas",
            info = "_START_–_END_ de _TOTAL_ linhas",
            paginate = list(previous = "Anterior", `next` = "Próxima")
          )
        )
      )
    })

    output$download_buttons <- shiny::renderUI({
      shiny::req(analysis_result())
      bslib::card(
        bslib::card_header("Downloads"),
        shiny::div(
          class = "download-grid",
          shiny::downloadButton(ns("download_complete_txt"), "Tabela completa"),
          shiny::downloadButton(ns("download_simplified_txt"), "Tabela simplificada"),
          shiny::downloadButton(ns("download_complete_xlsx"), "Excel completo"),
          shiny::downloadButton(ns("download_simplified_xlsx"), "Excel simplificado"),
          shiny::downloadButton(ns("download_complete_gpkg"), "GeoPackage"),
          shiny::downloadButton(ns("download_all"), "Todos os arquivos (.zip)")
        )
      )
    })

    copy_download <- function(key, default_name) {
      shiny::downloadHandler(
        filename = function() basename(analysis_result()$files[[key]] %||% default_name),
        content = function(file) file.copy(analysis_result()$files[[key]], file, overwrite = TRUE)
      )
    }
    output$download_complete_txt <- copy_download("complete_table", "resultado.txt")
    output$download_simplified_txt <- copy_download("simplified_table", "resultado_simplificado.txt")
    output$download_complete_xlsx <- copy_download("complete_xlsx", "resultado.xlsx")
    output$download_simplified_xlsx <- copy_download("simplified_xlsx", "resultado_simplificado.xlsx")
    output$download_complete_gpkg <- copy_download("complete_gpkg", "resultado.gpkg")
    output$download_all <- shiny::downloadHandler(
      filename = function() paste0(analysis_result()$output_name, "_arquivos.zip"),
      content = function(file) {
        files <- unname(analysis_result()$files[file.exists(analysis_result()$files)])
        zip::zipr(file, files = files, root = analysis_result()$output_folder)
      }
    )

    session$onSessionEnded(function() {
      process <- shiny::isolate(job_process())
      if (!is.null(process) && process$is_alive()) process$kill()
      safe_unlink(session_dir)
    })

    analysis_result
  })
}
