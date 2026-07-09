analysis_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 390,
      open = "desktop",
      bslib::accordion(
        id = ns("steps"),
        open = FALSE,
        bslib::accordion_panel(
          "1. Arquivo geoespacial",
          value = "step_geo",
          shiny::div(
            class = "geo-upload-dropzone",
            shiny::fileInput(
              ns("geo_upload"),
              label = "",
              multiple = TRUE,
              accept = c(
                ".gpkg", ".geojson", ".json", ".zip",
                ".shp", ".shx", ".dbf", ".prj", ".cpg", ".qpj", ".sbn", ".sbx",
                ".kml", ".gml"
              ),
              buttonLabel = "Selecionar ou arrastar arquivo(s)",
              placeholder = "Inserir o arquivo .gpkg, .shp, .zip, ou .json"
            ),
          ),
          shiny::uiOutput(ns("geo_validation_message")),
          shiny::selectizeInput(
            ns("group_column"),
            "Colunas de limites/grupos (até 2)",
            choices = NULL,
            multiple = TRUE,
            options = list(
              maxItems = 2,
              placeholder = "Nenhuma coluna = toda a área"
            )
          )
        ),
        bslib::accordion_panel(
          "2. Malha",
          value = "step_mesh",
          shiny::checkboxInput(ns("no_mesh"), "Não criar malha", FALSE),
          shiny::conditionalPanel(
            condition = sprintf("!input['%s']", ns("no_mesh")),
            shiny::numericInput(ns("mesh_size_km"), "Tamanho da quadrícula (km)", 5, min = 0.1, step = 0.1),
            shiny::sliderInput(
              ns("mesh_size_slider_km"),
              "Ajuste por controle",
              min = 0.1,
              max = 100,
              value = 5,
              step = 0.1,
              post = " km",
              ticks = FALSE
            ),
            shiny::helpText("Obs: A malha é criada em uma projeção métrica local e reprojetada para exibição no mapa."),
            shiny::uiOutput(ns("mesh_preview_notice"))
          )
        ),
        bslib::accordion_panel(
          "3. Rasters e classes",
          value = "step_rasters",
          shiny::div(
            class = "raster-upload-dropzone",
            shiny::fileInput(
              ns("raster_upload"),
              label = "",
              multiple = TRUE,
              accept = c(".tif", ".tiff", ".img", ".vrt", ".grd", ".zip"),
              buttonLabel = "Selecionar ou arrastar pasta/raster(s)",
              placeholder = "Inserir pasta ou arquivos .tif/.tiff com as imagens classificadas"
            )
          ),
          shiny::uiOutput(ns("raster_folder_display")),
          shiny::selectInput(
            ns("mapbiomas"),
            "Legenda das classes",
            choices = c(
              "Personalizar" = "custom",
              "MapBiomas - Coleção 4" = "4",
              "MapBiomas - Coleção 7.1" = "7.1",
              "MapBiomas - Coleção 8" = "8",
              "MapBiomas - Coleção 10" = "10"
            ),
            selected = "custom"
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] === 'custom'", ns("mapbiomas")),
            shiny::textInput(ns("class_forest"), "Floresta", ""),
            shiny::textInput(ns("class_nonforest"), "Não floresta", ""),
            shiny::textInput(ns("class_water"), "Água", ""),
            shiny::textInput(ns("class_others"), "Outros", ""),
            shiny::textInput(ns("class_agriculture"), "Agricultura", ""),
            shiny::textInput(ns("class_pasture"), "Pastagem", ""),
            shiny::textInput(ns("class_mining"), "Mineração", ""),
            shiny::textInput(ns("class_urban"), "Urbano", ""),
            shiny::div(
              class = "custom-classes-extra",
              shiny::tags$hr(),
              shiny::div(
                class = "custom-classes-header",
                shiny::strong("Classes adicionais"),
                shiny::actionButton(
                  ns("add_custom_class"),
                  "Adicionar classe",
                  icon = shiny::icon("plus"),
                  class = "btn-outline-primary btn-sm"
                )
              ),
              shiny::helpText("Para cada classe adicional, informe o nome da classe e os números correspondentes separados por vírgula."),
              shiny::uiOutput(ns("extra_custom_classes_ui"))
            )
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
            shiny::div(
              class = "results-table-header",
              shiny::div(
                class = "results-level-control",
                shiny::selectInput(
                  ns("result_level"),
                  "Nível exibido",
                  choices = c(
                    "Grid_ID" = "grid",
                    "Malha" = "mesh",
                    "Grupos" = "groups",
                    "Grupos 1" = "groups_1",
                    "Grupos 2" = "groups_2",
                    "Total" = "total"
                  ),
                  selected = "grid"
                )
              ),
              shiny::downloadButton(
                ns("download_all"),
                "Download resultados (.zip)",
                class = "btn-primary results-download-button"
              )
            )
          ),
          DT::DTOutput(ns("result_table"))
        )
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
    geo_preview <- shiny::reactiveVal(NULL)
    geo_invalid_geometry <- shiny::reactiveVal(FALSE)
    analysis_result <- shiny::reactiveVal(NULL)
    validation_state <- shiny::reactiveVal(NULL)
    geo_validation_state <- shiny::reactiveVal(NULL)
    raster_validation_state <- shiny::reactiveVal(NULL)
    pixel_area_value <- shiny::reactiveVal(NULL)
    pixel_area_summary <- shiny::reactiveVal("—")
    raster_folder_path <- shiny::reactiveVal("")
    raster_folder_label <- shiny::reactiveVal("")
    raster_validation_token <- shiny::reactiveVal(0L)
    raster_validation_process <- shiny::reactiveVal(NULL)
    raster_validation_progress_file <- shiny::reactiveVal(NULL)
    raster_validation_result_file <- shiny::reactiveVal(NULL)
    raster_validation_job_token <- shiny::reactiveVal(NULL)
    geo_validation_token <- shiny::reactiveVal(0L)
    geo_validation_process <- shiny::reactiveVal(NULL)
    geo_validation_progress_file <- shiny::reactiveVal(NULL)
    geo_validation_result_file <- shiny::reactiveVal(NULL)
    geo_validation_job_token <- shiny::reactiveVal(NULL)
    running <- shiny::reactiveVal(FALSE)
    job_process <- shiny::reactiveVal(NULL)
    job_progress_file <- shiny::reactiveVal(NULL)
    job_result_file <- shiny::reactiveVal(NULL)
    progress_state <- shiny::reactiveVal(list(percent = 0, stage = "Aguardando", detail = "", status = "idle"))
    extra_custom_class_ids <- shiny::reactiveVal(integer())
    next_extra_custom_class_id <- shiny::reactiveVal(0L)

    set_raster_validation_progress <- function(percent, stage, detail = NULL, status = "running") {
      raster_validation_state(list(
        status = status,
        percent = max(0, min(100, as.numeric(percent))),
        stage = as.character(stage),
        detail = as.character(detail %||% "")
      ))
      try(get("flushReact", asNamespace("shiny"))(), silent = TRUE)
      invisible(NULL)
    }

    set_geo_validation_progress <- function(percent, stage, detail = NULL, status = "running") {
      geo_validation_state(list(
        status = status,
        percent = max(0, min(100, as.numeric(percent))),
        stage = as.character(stage),
        detail = as.character(detail %||% "")
      ))
      try(get("flushReact", asNamespace("shiny"))(), silent = TRUE)
      invisible(NULL)
    }

    toggle_run_button <- function(disabled) {
      session$sendCustomMessage(
        "toggle-disabled",
        list(id = ns("run"), disabled = isTRUE(disabled))
      )
    }

    geospatial_validation_running <- function() {
      process <- geo_validation_process()
      !is.null(process) && process$is_alive()
    }

    geospatial_ready <- function() {
      !geospatial_validation_running() &&
        !is.null(geo_data()) &&
        !isTRUE(geo_invalid_geometry())
    }

    mark_invalid_geospatial_file <- function(clear_preview = TRUE) {
      if (isTRUE(clear_preview)) {
        geo_path(NULL)
        geo_preview(NULL)
        shiny::updateSelectizeInput(session, "group_column", choices = NULL, selected = character(), server = TRUE)
      }
      geo_data(NULL)
      geo_invalid_geometry(TRUE)
      validation_state(list(type = "danger", text = invalid_geometry_message()))
      toggle_run_button(TRUE)
      invisible(NULL)
    }

    normalize_custom_class_name <- function(name) {
      name <- trimws(as.character(name %||% ""))
      name <- gsub("[\r\n\t]+", " ", name)
      name <- gsub(" {2,}", " ", name)
      if (!nzchar(name)) return("")
      numeric_name <- suppressWarnings(as.numeric(name))
      if (!is.na(numeric_name) || grepl("^Variation_[0-9]+$", name)) {
        name <- paste0("Class_", name)
      }
      name
    }

    shiny::observeEvent(input$add_custom_class, {
      id <- next_extra_custom_class_id() + 1L
      next_extra_custom_class_id(id)
      extra_custom_class_ids(c(extra_custom_class_ids(), id))

      local({
        this_id <- id
        shiny::observeEvent(input[[paste0("remove_extra_custom_class_", this_id)]], {
          extra_custom_class_ids(setdiff(extra_custom_class_ids(), this_id))
        }, ignoreInit = TRUE, once = TRUE)
      })
    }, ignoreInit = TRUE)

    output$extra_custom_classes_ui <- shiny::renderUI({
      ids <- extra_custom_class_ids()
      if (!length(ids)) return(NULL)
      shiny::tagList(lapply(ids, function(id) {
        shiny::div(
          class = "custom-class-card",
          shiny::div(
            class = "custom-class-card-header",
            shiny::strong(paste("Classe adicional", match(id, ids))),
            shiny::actionButton(
              ns(paste0("remove_extra_custom_class_", id)),
              "Remover",
              icon = shiny::icon("trash"),
              class = "btn-outline-danger btn-sm"
            )
          ),
          shiny::textInput(
            ns(paste0("extra_custom_class_name_", id)),
            "Nome da classe",
            value = ""
          ),
          shiny::textInput(
            ns(paste0("extra_custom_class_values_", id)),
            "Números relacionados à classe",
            value = "",
            placeholder = "Ex.: 12, 13, 14"
          )
        )
      }))
    })

    stop_raster_validation_process <- function() {
      process <- shiny::isolate(raster_validation_process())
      if (!is.null(process) && process$is_alive()) {
        try(process$kill(), silent = TRUE)
      }
      raster_validation_process(NULL)
      raster_validation_progress_file(NULL)
      raster_validation_result_file(NULL)
      raster_validation_job_token(NULL)
      invisible(NULL)
    }

    apply_raster_inspection <- function(inspection) {
      pixel_area_value(inspection$pixel_area_km2)
      pixel_area_summary(paste0(
        format(inspection$pixel_area_km2, scientific = FALSE, digits = 8, decimal.mark = ","),
        " km²",
        if (isTRUE(inspection$pixel_area_standardized)) {
          paste0(" (", inspection$pixel_area_standard_label, ")")
        } else {
          ""
        }
      ))
      consistency <- c(
        if (inspection$same_crs) "CRS consistente" else "CRS diferentes",
        if (inspection$same_resolution) "resolução consistente" else "resoluções diferentes"
      )
      raster_validation_state(list(
        status = if (inspection$same_crs && inspection$same_resolution) "complete" else "warning",
        percent = 100,
        type = if (inspection$same_crs && inspection$same_resolution) "success" else "warning",
        text = paste0(
          "Validação concluída: ", inspection$count, " raster(s), anos ",
          inspection$years[[1]], "–", inspection$years[[2]], "; ",
          paste(consistency, collapse = "; "), ". Resolução: ",
          paste(format(inspection$resolution, digits = 7), collapse = " × "), "."
        )
      ))
      invisible(inspection)
    }

    schedule_uploaded_raster_validation <- function(upload, token) {
      stop_raster_validation_process()

      upload_dir <- file.path(session_dir, paste0("rasters_", token, "_", as.integer(Sys.time())))
      progress_file <- file.path(session_dir, paste0("raster_validation_", token, ".json"))
      result_file <- file.path(session_dir, paste0("raster_validation_", token, ".rds"))
      safe_unlink(c(progress_file, result_file))
      write_progress(progress_file, 1, "Carregando arquivos", "Preparando validação em segundo plano.", "running")

      process <- callr::r_bg(
        func = function(upload, upload_dir, progress_file, result_file, app_directory) {
          source(file.path(app_directory, "R", "utils.R"), local = globalenv())

          progress <- function(percent, stage, detail = NULL, status = "running") {
            write_progress(progress_file, percent, stage, detail, status)
          }
          scaled_validation_progress <- function(percent, stage, detail = NULL, status = "running") {
            progress(20 + (max(0, min(100, as.numeric(percent))) / 100) * 78, stage, detail, status)
          }

          tryCatch({
            progress(1, "Carregando arquivos", "Copiando arquivos enviados para a sessão.")
            staged_folder <- stage_raster_upload(upload, upload_dir, progress = progress)
            inspection <- inspect_raster_folder(staged_folder, progress = scaled_validation_progress)
            saveRDS(
              list(
                staged_folder = normalizePath(staged_folder, mustWork = TRUE),
                label = paste0("Upload: ", nrow(upload), " arquivo(s) enviado(s)"),
                inspection = inspection
              ),
              result_file
            )
            progress(100, "Validação concluída", "Rasters carregados e validados.", "complete")
          }, error = function(e) {
            write_progress(progress_file, 100, "Erro", conditionMessage(e), "error")
            stop(e)
          })
        },
        args = list(
          upload = upload,
          upload_dir = upload_dir,
          progress_file = progress_file,
          result_file = result_file,
          app_directory = app_root()
        ),
        supervise = TRUE,
        stdout = "|",
        stderr = "|"
      )

      raster_validation_process(process)
      raster_validation_progress_file(progress_file)
      raster_validation_result_file(result_file)
      raster_validation_job_token(token)
      invisible(process)
    }

    stop_geospatial_validation_process <- function() {
      process <- shiny::isolate(geo_validation_process())
      if (!is.null(process) && process$is_alive()) {
        try(process$kill(), silent = TRUE)
      }
      geo_validation_process(NULL)
      geo_validation_progress_file(NULL)
      geo_validation_result_file(NULL)
      geo_validation_job_token(NULL)
      invisible(NULL)
    }

    update_mesh_recommendation_inputs <- function(mesh_recommendation) {
      if (is.null(mesh_recommendation) || !is.finite(mesh_recommendation$size_km)) {
        return(invisible(NULL))
      }
      shiny::updateNumericInput(
        session,
        "mesh_size_km",
        value = mesh_recommendation$size_km
      )
      shiny::updateSliderInput(
        session,
        "mesh_size_slider_km",
        min = 0.1,
        max = max(100, mesh_recommendation$size_km),
        value = mesh_recommendation$size_km
      )
      invisible(mesh_recommendation)
    }

    mesh_recommendation_message <- function(mesh_recommendation) {
      if (is.null(mesh_recommendation) || !isTRUE(mesh_recommendation$adjusted)) return("")
      estimated_mesh_text <- if (is.finite(mesh_recommendation$estimated_cells)) {
        paste0(
          " (aprox. ",
          format(round(mesh_recommendation$estimated_cells), big.mark = ".", decimal.mark = ","),
          " quadrículas pela extensão)."
        )
      } else {
        "."
      }
      paste0(
        " Malha inicial sugerida automaticamente: ",
        format(mesh_recommendation$size_km, big.mark = ".", decimal.mark = ","),
        " km",
        estimated_mesh_text
      )
    }

    apply_geospatial_validation <- function(result, source_label = NULL) {
      geo <- result$geo
      geo_data(geo)
      geo_invalid_geometry(FALSE)
      columns <- result$columns %||% names(sf::st_drop_geometry(geo))
      selected_group_columns <- intersect(shiny::isolate(input$group_column) %||% character(), columns)
      mesh_recommendation <- result$mesh_recommendation
      update_mesh_recommendation_inputs(mesh_recommendation)
      shiny::updateSelectizeInput(
        session,
        "group_column",
        choices = stats::setNames(columns, columns),
        selected = selected_group_columns,
        server = TRUE
      )
      message <- paste0(
        "Arquivo geoespacial validado e pronto para análise",
        if (!is.null(source_label) && nzchar(source_label)) paste0(": ", source_label) else "",
        "."
      )
      geo_validation_state(list(status = "complete", type = "success", text = message))
      validation_state(list(type = "success", text = message))
      if (!isTRUE(shiny::isolate(running()))) toggle_run_button(FALSE)
      invisible(result)
    }

    schedule_geospatial_validation <- function(path, source_label, token) {
      stop_geospatial_validation_process()

      progress_file <- file.path(session_dir, paste0("geo_validation_", token, ".json"))
      result_file <- file.path(session_dir, paste0("geo_validation_", token, ".rds"))
      safe_unlink(c(progress_file, result_file))
      write_progress(progress_file, 20, "Prévia pronta", "Validando geometria em segundo plano.", "running")

      process <- callr::r_bg(
        func = function(path, source_label, progress_file, result_file, app_directory) {
          source(file.path(app_directory, "R", "utils.R"), local = globalenv())
          source(file.path(app_directory, "R", "spatial_io.R"), local = globalenv())

          progress <- function(percent, stage, detail = NULL, status = "running") {
            write_progress(progress_file, percent, stage, detail, status)
          }

          tryCatch({
            geo <- read_geo(path, progress = progress)
            progress(88, "Estimativa da malha", "Calculando sugestão pela extensão do arquivo.")
            mesh_recommendation <- recommend_mesh_size_km(
              geo,
              target_cells = 1000L,
              max_cells = 20000L
            )
            progress(94, "Resumo do arquivo", "Preparando atributos e resumo geoespacial.")
            saveRDS(
              list(
                geo = geo,
                columns = names(sf::st_drop_geometry(geo)),
                mesh_recommendation = mesh_recommendation,
                source_label = source_label
              ),
              result_file
            )
            progress(100, "Validação concluída", "Arquivo pronto para análise.", "complete")
          }, error = function(e) {
            write_progress(progress_file, 100, "Erro", conditionMessage(e), "error")
            stop(e)
          })
        },
        args = list(
          path = path,
          source_label = source_label,
          progress_file = progress_file,
          result_file = result_file,
          app_directory = app_root()
        ),
        supervise = TRUE,
        stdout = "|",
        stderr = "|"
      )

      geo_validation_process(process)
      geo_validation_progress_file(progress_file)
      geo_validation_result_file(result_file)
      geo_validation_job_token(token)
      invisible(process)
    }

    load_geospatial_file <- function(path, source_label = NULL, token = NULL) {
      token <- token %||% shiny::isolate(geo_validation_token())
      geo_path(path)
      geo_data(NULL)
      geo_invalid_geometry(FALSE)
      toggle_run_button(TRUE)
      set_geo_validation_progress(3, "Leitura do arquivo", "Abrindo prévia rápida para o mapa.")
      preview <- read_geo_preview(path, simplify_tolerance_m = 250)
      geo_preview(preview)
      preview_is_bbox <- identical(attr(preview, "landscript_preview_type"), "bbox")
      columns <- if (isTRUE(preview_is_bbox)) {
        character()
      } else {
        names(sf::st_drop_geometry(preview))
      }
      selected_group_columns <- intersect(shiny::isolate(input$group_column) %||% character(), columns)
      mesh_recommendation <- recommend_mesh_size_km(
        preview,
        target_cells = 1000L,
        max_cells = 20000L
      )
      update_mesh_recommendation_inputs(mesh_recommendation)
      shiny::updateSelectizeInput(
        session,
        "group_column",
        choices = stats::setNames(columns, columns),
        selected = selected_group_columns,
        server = TRUE
      )
      set_geo_validation_progress(
        20,
        "Prévia pronta",
        if (isTRUE(preview_is_bbox)) {
          "Mapa liberado pela extensão do arquivo; validação completa em segundo plano."
        } else {
          "Mapa liberado; validação completa em segundo plano."
        }
      )
      validation_state(list(
        type = "info",
        text = paste0(
          "Prévia do arquivo geoespacial carregada",
          if (!is.null(source_label) && nzchar(source_label)) paste0(": ", source_label) else "",
          ". Aguarde a validação completa para rodar a análise.",
          mesh_recommendation_message(mesh_recommendation)
        )
      ))
      schedule_geospatial_validation(path, source_label, token)
    }

    shiny::observeEvent(input$raster_upload, {
      upload <- input$raster_upload
      if (is.null(upload) || !nrow(upload)) return(NULL)
      token <- raster_validation_token() + 1L
      raster_validation_token(token)
      raster_folder_path("")
      raster_folder_label("Upload: preparando arquivos enviados...")
      validation_state(NULL)
      pixel_area_value(NULL)
      pixel_area_summary("—")
      toggle_run_button(TRUE)
      set_raster_validation_progress(
        1,
        "Carregando arquivos",
        "Arquivos recebidos pelo Shiny. Preparando validação."
      )
      schedule_uploaded_raster_validation(upload, token)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$clear_raster_folder, {
      raster_validation_token(raster_validation_token() + 1L)
      stop_raster_validation_process()
      raster_folder_path("")
      raster_folder_label("")
      validation_state(NULL)
      raster_validation_state(NULL)
      pixel_area_value(NULL)
      pixel_area_summary("—")
    })

    output$raster_folder_display <- shiny::renderUI({
      path <- raster_folder_path()
      label <- raster_folder_label()
      if (!nzchar(path) && !nzchar(label)) {
        return(shiny::div(
          class = "folder-selection folder-selection-empty",
          shiny::icon("folder-open"),
          shiny::span("Nenhuma pasta selecionada")
        ))
      }
      shiny::div(
        class = "folder-selection",
        shiny::icon("folder"),
        shiny::span(label %||% path, title = path)
      )
    })

    shiny::observeEvent(input$geo_upload, {
      upload <- input$geo_upload
      if (is.null(upload) || !nrow(upload)) return(NULL)
      token <- geo_validation_token() + 1L
      geo_validation_token(token)
      stop_geospatial_validation_process()
      geo_path(NULL)
      geo_data(NULL)
      geo_preview(NULL)
      geo_invalid_geometry(FALSE)
      validation_state(NULL)
      set_geo_validation_progress(1, "Recebendo arquivo", "Preparando arquivo enviado.")
      toggle_run_button(TRUE)

      upload_snapshot <- upload
      source_label <- paste(upload$name, collapse = ", ")
      later::later(function() {
        if (!identical(token, shiny::isolate(geo_validation_token()))) return(NULL)
        tryCatch({
          set_geo_validation_progress(2, "Preparando arquivo", "Copiando arquivo para a sessão.")
          upload_dir <- file.path(session_dir, paste0("geo_", as.integer(Sys.time())))
          path <- stage_uploaded_vector(upload_snapshot, upload_dir)
          load_geospatial_file(path, source_label, token)
        }, error = function(e) {
          if (identical(conditionMessage(e), invalid_geometry_message())) {
            mark_invalid_geospatial_file()
          } else {
            geo_path(NULL)
            geo_data(NULL)
            geo_preview(NULL)
            geo_invalid_geometry(FALSE)
            geo_validation_state(list(status = "error", type = "danger", text = conditionMessage(e)))
            validation_state(list(type = "danger", text = conditionMessage(e)))
            if (!isTRUE(shiny::isolate(running()))) toggle_run_button(TRUE)
          }
        })
      }, delay = 0.05)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$mesh_size_km, {
      value <- input$mesh_size_km
      if (!is.null(value) && is.finite(value) && value > 0 &&
          !isTRUE(all.equal(value, input$mesh_size_slider_km))) {
        shiny::updateSliderInput(
          session,
          "mesh_size_slider_km",
          min = min(0.1, value),
          max = max(100, value),
          value = value
        )
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$mesh_size_slider_km, {
      value <- input$mesh_size_slider_km
      if (!is.null(value) && !isTRUE(all.equal(value, input$mesh_size_km))) {
        shiny::updateNumericInput(session, "mesh_size_km", value = value)
      }
    }, ignoreInit = TRUE)

    mesh_size_debounced <- shiny::debounce(
      shiny::reactive(input$mesh_size_km),
      millis = 650
    )

    mesh_preview_notice <- shiny::reactiveVal(NULL)

    preview_mesh <- shiny::reactive({
      geo <- geo_preview() %||% geo_data()
      shiny::req(geo)
      mesh_preview_notice(NULL)
      if (!isTRUE(input$no_mesh)) {
        size_km <- mesh_size_debounced()
        shiny::validate(shiny::need(!is.null(size_km) && is.finite(size_km) && size_km > 0, "O tamanho da malha deve ser positivo."))
        estimated_cells <- estimate_mesh_cells_km(geo, size_km)
        preview_limit <- 20000L
        if (is.finite(estimated_cells) && estimated_cells > preview_limit) {
          mesh_preview_notice(paste0(
            "Pré-visualização da malha omitida: esta configuração estima ",
            format(round(estimated_cells), big.mark = ".", decimal.mark = ","),
            " quadrículas. Aumente o tamanho da quadrícula para visualizar com mais fluidez."
          ))
          return(NULL)
        }
        size <- size_km * 1000
      } else {
        size <- NULL
      }
      create.mesh(
        geo,
        mesh.size = size,
        group.column = ".ALL",
        max.cells = 20000L,
        mesh.unit = "meters"
      )
    })

    output$mesh_preview_notice <- shiny::renderUI({
      notice <- mesh_preview_notice()
      if (is.null(notice) || !nzchar(notice)) return(NULL)
      app_alert(notice, color = "warning")
    })

    output$preview_map <- leaflet::renderLeaflet({
      geo <- geo_preview() %||% geo_data()
      map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satélite (Esri)",
          options = leaflet::providerTileOptions(maxZoom = 20)
        ) |>
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "Ruas") |>
        leaflet::hideGroup("Ruas")

      if (is.null(geo)) return(map |> leaflet::setView(lng = -54, lat = -12, zoom = 4))

      mesh <- if (!is.null(geo_data())) {
        tryCatch(preview_mesh(), error = function(e) NULL)
      } else {
        NULL
      }
      bbox <- sf::st_bbox(geo)
      map <- map |>
        leaflet::addPolygons(
          data = geo,
          fill = FALSE,
          color = "#00E5FF",
          weight = 3,
          opacity = 1,
          group = "Prévia do limite"
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
          overlayGroups = c("Prévia do limite", "Malha"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    })

    output$geo_summary <- shiny::renderTable({
      geo <- geo_data() %||% geo_preview()
      shiny::req(geo)
      rbind(
        geo_summary(geo),
        data.frame(
          item = "Área do pixel estimada",
          value = pixel_area_summary(),
          stringsAsFactors = FALSE
        )
      )
    }, striped = TRUE, bordered = FALSE, spacing = "s")

    output$attribute_table <- DT::renderDT({
      geo <- geo_data() %||% geo_preview()
      shiny::req(geo)
      DT::datatable(
        utils::head(sf::st_drop_geometry(geo), 200),
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
      parsed <- lapply(values, function(x) {
        if (!nzchar(trimws(x %||% ""))) return(numeric())
        parse_number_vector(x, integer = TRUE)
      })

      extra_ids <- extra_custom_class_ids()
      if (length(extra_ids)) {
        for (id in extra_ids) {
          raw_name <- input[[paste0("extra_custom_class_name_", id)]] %||% ""
          raw_values <- input[[paste0("extra_custom_class_values_", id)]] %||% ""
          class_name <- normalize_custom_class_name(raw_name)
          has_name <- nzchar(class_name)
          has_values <- nzchar(trimws(raw_values))

          if (!has_name && !has_values) {
            next
          }
          if (!has_name) {
            stop("Informe o nome da classe adicional.", call. = FALSE)
          }
          if (!has_values) {
            stop("Informe os números relacionados à classe adicional '", class_name, "'.", call. = FALSE)
          }

          parsed[[class_name]] <- parse_number_vector(raw_values, integer = TRUE)
        }
      }

      names(parsed) <- make.unique(names(parsed), sep = "_")
      parsed[lengths(parsed) > 0L]
    })

    validate_rasters <- function() {
      set_raster_validation_progress(1, "Carregando arquivos", "Acessando a pasta selecionada.")
      folder <- shiny::isolate(raster_folder_path())
      inspection <- inspect_raster_folder(
        folder,
        progress = set_raster_validation_progress
      )
      apply_raster_inspection(inspection)
      try(get("flushReact", asNamespace("shiny"))(), silent = TRUE)
      inspection
    }

    shiny::observe({
      process <- geo_validation_process()
      if (is.null(process)) return()
      shiny::invalidateLater(350, session)

      token <- geo_validation_job_token()
      if (!identical(token, shiny::isolate(geo_validation_token()))) return()

      progress_file <- geo_validation_progress_file()
      result_file <- geo_validation_result_file()
      if (!is.null(progress_file) && file.exists(progress_file)) {
        progress <- read_progress(progress_file)
        geo_validation_state(list(
          status = progress$status %||% "running",
          percent = progress$percent %||% 0,
          stage = progress$stage %||% "Validando geometria",
          detail = progress$detail %||% ""
        ))
      }

      if (!process$is_alive()) {
        exit_status <- process$get_exit_status()
        geo_validation_process(NULL)
        geo_validation_progress_file(NULL)
        geo_validation_result_file(NULL)
        geo_validation_job_token(NULL)

        if (identical(exit_status, 0L) && !is.null(result_file) && file.exists(result_file)) {
          result <- readRDS(result_file)
          apply_geospatial_validation(result, result$source_label %||% NULL)
        } else {
          error_lines <- c(process$read_error_lines(), process$read_output_lines())
          progress <- if (!is.null(progress_file)) read_progress(progress_file) else list()
          detail <- progress$detail %||% utils::tail(error_lines[nzchar(error_lines)], 1L) %||% "Falha desconhecida ao validar a geometria."
          geo_data(NULL)
          geo_invalid_geometry(TRUE)
          geo_validation_state(list(status = "error", type = "danger", text = detail))
          validation_state(list(type = "danger", text = detail))
          toggle_run_button(TRUE)
        }
      }
    })

    shiny::observe({
      process <- raster_validation_process()
      if (is.null(process)) return()
      shiny::invalidateLater(350, session)

      token <- raster_validation_job_token()
      if (!identical(token, shiny::isolate(raster_validation_token()))) return()

      progress_file <- raster_validation_progress_file()
      result_file <- raster_validation_result_file()
      if (!is.null(progress_file) && file.exists(progress_file)) {
        progress <- read_progress(progress_file)
        raster_validation_state(list(
          status = progress$status %||% "running",
          percent = progress$percent %||% 0,
          stage = progress$stage %||% "Validando rasters",
          detail = progress$detail %||% ""
        ))
      }

      if (!process$is_alive()) {
        exit_status <- process$get_exit_status()
        raster_validation_process(NULL)
        raster_validation_progress_file(NULL)
        raster_validation_result_file(NULL)
        raster_validation_job_token(NULL)

        if (identical(exit_status, 0L) && !is.null(result_file) && file.exists(result_file)) {
          result <- readRDS(result_file)
          raster_folder_path(result$staged_folder)
          raster_folder_label(result$label)
          apply_raster_inspection(result$inspection)
          if (!isTRUE(running()) && isTRUE(geospatial_ready())) toggle_run_button(FALSE)
        } else {
          error_lines <- c(process$read_error_lines(), process$read_output_lines())
          progress <- if (!is.null(progress_file)) read_progress(progress_file) else list()
          detail <- progress$detail %||% utils::tail(error_lines[nzchar(error_lines)], 1L) %||% "Falha desconhecida ao validar os rasters."
          raster_folder_path("")
          raster_folder_label("")
          pixel_area_value(NULL)
          pixel_area_summary("—")
          raster_validation_state(list(status = "error", type = "danger", text = detail))
          if (!isTRUE(running()) && isTRUE(geospatial_ready())) toggle_run_button(FALSE)
        }
      }
    })

    resume_proxy_available <- function() {
      output_folder <- path.expand(input$output_folder %||% "")
      output_name <- sanitize_output_name(input$output_name %||% "LandScript_result")
      if (!nzchar(output_folder) || !nzchar(output_name)) return(FALSE)
      proxy_dir <- analysis_proxy_dir(output_folder, output_name)
      file.exists(proxy_checkpoint_path(proxy_dir, "params"))
    }

    validate_parameters <- function() {
      if (isTRUE(geo_invalid_geometry())) {
        validation_state(list(type = "danger", text = invalid_geometry_message()))
        toggle_run_button(TRUE)
        stop(invalid_geometry_message(), call. = FALSE)
      }
      if (isTRUE(geospatial_validation_running())) {
        stop("Aguarde a validação do arquivo geoespacial terminar antes de rodar o algoritmo.", call. = FALSE)
      }
      raster_process <- raster_validation_process()
      if (!is.null(raster_process) && raster_process$is_alive()) {
        stop("Aguarde a validação dos rasters terminar antes de rodar o algoritmo.", call. = FALSE)
      }
      resume_only <- resume_proxy_available() &&
        (is.null(geo_path()) || is.null(geo_data()) || !nzchar(raster_folder_path()))
      if (isTRUE(resume_only)) {
        output_folder <- ensure_output_folder(input$output_folder)
        return(list(rasters = NULL, output_folder = output_folder, resume_proxy = TRUE))
      }
      shiny::req(geo_path(), geo_data())
      if (!isTRUE(input$no_mesh) &&
          (is.null(input$mesh_size_km) || !is.finite(input$mesh_size_km) || input$mesh_size_km <= 0)) {
        stop("O tamanho da malha deve ser um número positivo.", call. = FALSE)
      }
      rasters <- list_raster_files(raster_folder_path())
      output_folder <- ensure_output_folder(input$output_folder)
      if (is.null(pixel_area_value()) || !is.finite(pixel_area_value()) || pixel_area_value() <= 0) {
        stop("Selecione uma pasta de rasters válida para estimar a área do pixel.", call. = FALSE)
      }
      mapbiomas_class_map(input$mapbiomas, custom_classes())
      list(rasters = rasters, output_folder = output_folder, resume_proxy = FALSE)
    }

    progress_status_ui <- function(message, default_stage, container_class) {
      if (is.null(message)) return(NULL)
      if (!is.null(message$percent) && is.null(message$text)) {
        percent <- max(0, min(100, as.numeric(message$percent %||% 0)))
        color <- if (identical(message$status, "error")) "danger" else {
          if (identical(message$status, "complete")) "success" else "primary"
        }
        return(shiny::div(
          class = container_class,
          shiny::div(
            class = "progress-status raster-progress-status",
            shiny::strong(message$stage %||% default_stage),
            shiny::tags$small(class = "text-muted", message$detail %||% "")
          ),
          shiny::div(
            class = "progress raster-progress",
            role = "progressbar",
            `aria-valuenow` = percent,
            `aria-valuemin` = 0,
            `aria-valuemax` = 100,
            shiny::div(
              class = paste("progress-bar progress-bar-striped progress-bar-animated", paste0("bg-", color)),
              style = paste0("width:", percent, "%"),
              paste0(round(percent), "%")
            )
          )
        ))
      }
      shiny::div(
        class = container_class,
        app_alert(message$text, color = message$type, dismissible = TRUE)
      )
    }

    output$geo_validation_message <- shiny::renderUI({
      progress_status_ui(
        geo_validation_state(),
        "Validando geometria",
        "geo-validation-result"
      )
    })

    output$raster_validation_message <- shiny::renderUI({
      progress_status_ui(
        raster_validation_state(),
        "Validando rasters",
        "raster-validation-result"
      )
    })

    output$validation_message <- shiny::renderUI({
      message <- validation_state()
      if (is.null(message)) return(NULL)
      app_alert(message$text, color = message$type, dismissible = TRUE)
    })

    shiny::observeEvent(input$run, {
      if (running()) return()
      tryCatch({
        validated <- validate_parameters()
        resume_proxy <- isTRUE(validated$resume_proxy)
        params <- list(
          geo_path = if (resume_proxy) NULL else geo_path(),
          group_column = input$group_column,
          no_mesh = isTRUE(input$no_mesh),
          mesh_size = if (isTRUE(input$no_mesh)) NULL else input$mesh_size_km * 1000,
          mesh_unit = "meters",
          raster_folder = if (resume_proxy) NULL else normalizePath(raster_folder_path(), mustWork = TRUE),
          output_folder = path.expand(input$output_folder),
          output_name = sanitize_output_name(input$output_name),
          mapbiomas = input$mapbiomas,
          custom_classes = custom_classes(),
          pixel_km2_ratio = if (resume_proxy) NA_real_ else pixel_area_value(),
          max_cells = 1000000L,
          resume_proxy = resume_proxy
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
      level <- input$result_level
      if (is.null(level) || length(level) != 1L || !nzchar(level)) {
        level <- if ("grid" %in% names(result)) "grid" else "mesh"
      }
      available_levels <- c("grid", "mesh", "groups", "groups_1", "groups_2", "total")
      if (!level %in% available_levels || is.null(result[[level]])) {
        level <- if ("grid" %in% names(result)) "grid" else "mesh"
      }
      result[[level]]
    })

    output$result_summary <- shiny::renderUI({
      result <- analysis_result()
      shiny::req(result)
      group_count <- 1L
      group_columns <- normalize_group_columns(result$group_column %||% character(), max_columns = 2L)
      if (!is.null(result$groups) && length(group_columns) &&
          all(group_columns %in% names(result$groups))) {
        group_table <- unique(sf::st_drop_geometry(result$groups)[group_columns])
        group_count <- nrow(group_table)
      }
      year_min <- if (!is.null(result$raster_index$year) && length(result$raster_index$year)) {
        min(result$raster_index$year, na.rm = TRUE)
      } else {
        NA
      }
      year_max <- if (!is.null(result$raster_index$year) && length(result$raster_index$year)) {
        max(result$raster_index$year, na.rm = TRUE)
      } else {
        NA
      }
      shiny::tagList(
        if (isTRUE(result$overlap_removed)) {
          app_alert(
            "Foram detectadas sobreposições entre grupos. Cada área sobreposta foi atribuída a apenas um grupo para evitar dupla contagem.",
            color = "warning"
          )
        },
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          bslib::value_box(
            "Unidades da malha",
            format(nrow(result$mesh), big.mark = ".", decimal.mark = ","),
            showcase = shiny::icon("border-all"),
            theme = "primary"
          ),
          bslib::value_box(
            "Grupos",
            format(
              group_count,
              big.mark = ".",
              decimal.mark = ","
            ),
            showcase = shiny::icon("layer-group"),
            theme = "info"
          ),
          bslib::value_box("Anos", paste0(year_min, "–", year_max), showcase = shiny::icon("calendar"), theme = "success"),
          bslib::value_box("Rasters", nrow(result$raster_index %||% data.frame()), showcase = shiny::icon("images"), theme = "warning")
        )
      )
    })

    output$result_table <- DT::renderDT({
      data <- selected_result_data()
      table_data <- result_table_data(data)
      DT::datatable(
        table_data,
        rownames = FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          dom = "frtip",
          language = list(
            search = "Pesquisar:",
            lengthMenu = "Mostrar _MENU_ linhas",
            info = "_START_–_END_ de _TOTAL_ linhas",
            paginate = list(previous = "Anterior", `next` = "Próxima")
          )
        )
      )
    })

    output$download_all <- shiny::downloadHandler(
      filename = function() {
        result <- analysis_result()
        shiny::req(result)
        paste0(result$output_name, "_resultados.zip")
      },
      content = function(file) {
        result <- analysis_result()
        prepare_result_download_zip(result, file)
      }
    )

    session$onSessionEnded(function() {
      geo_process <- shiny::isolate(geo_validation_process())
      if (!is.null(geo_process) && geo_process$is_alive()) geo_process$kill()
      raster_process <- shiny::isolate(raster_validation_process())
      if (!is.null(raster_process) && raster_process$is_alive()) raster_process$kill()
      process <- shiny::isolate(job_process())
      if (!is.null(process) && process$is_alive()) process$kill()
      safe_unlink(session_dir)
    })

    analysis_result
  })
}
