analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  step_title <- function(label, status_output_id) {
    shiny::span(
      class = "analysis-step-title",
      shiny::span(label),
      shiny::uiOutput(ns(status_output_id), inline = TRUE)
    )
  }

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 390,
      open = "desktop",
      bslib::accordion(
        id = ns("steps"),
        open = FALSE,
        multiple = FALSE,
        bslib::accordion_panel(
          step_title("1. Arquivo geoespacial e malha", "step_geo_mesh_status"),
          value = "step_geo_mesh",
          shiny::div(
            class = "geo-upload-dropzone",
            shiny::fileInput(
              ns("geo_upload"),
              label = "",
              multiple = TRUE,
              accept = c(
                ".gpkg", ".geojson", ".json", ".zip",
                ".shp", ".shx", ".dbf", ".prj", ".cpg", ".qpj", ".sbn", ".sbx",
                ".xml", ".kml", ".gml"
              ),
              buttonLabel = "Selecionar ou arrastar arquivo(s)",
              placeholder = "Inserir o arquivo .gpkg, .shp, .zip, ou .json"
            )
          ),
          shinyFiles::shinyFilesButton(
            ns("geo_local_select"),
            "Selecionar arquivo local...",
            "Escolha um GeoPackage, shapefile, GeoJSON ou ZIP",
            multiple = FALSE,
            class = "btn-outline-primary w-100 local-vector-button",
            icon = shiny::icon("folder-open")
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
          ),
          shiny::tags$hr(class = "step-section-divider"),
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
          step_title("2. Rasters e classes", "step_rasters_status"),
          value = "step_rasters",
          shinyFiles::shinyDirButton(
            ns("raster_local_select"),
            "Selecionar pasta de rasters...",
            "Escolha a pasta local com os rasters classificados",
            class = "btn-primary w-100 raster-local-button",
            icon = shiny::icon("folder-open")
          ),
          shiny::helpText(
            "Os rasters são lidos diretamente da pasta selecionada e não são copiados para o disco interno."
          ),
          shiny::uiOutput(ns("raster_folder_display")),
          shiny::selectInput(
            ns("mapbiomas"),
            "Legenda das classes",
            choices = c(
              "Sem legenda" = "none",
              "Personalizar" = "custom",
              "MapBiomas - Coleção 4" = "4",
              "MapBiomas - Coleção 7.1" = "7.1",
              "MapBiomas - Coleção 8" = "8",
              "MapBiomas - Coleção 10" = "10"
            ),
            selected = "none"
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
          step_title("3. Saída e execução", "step_output_status"),
          value = "step_output",
          shiny::div(
            class = "output-folder-picker",
            shiny::textInput(ns("output_folder"), "Pasta de saída", value = file.path(getwd(), "Results")),
            shinyFiles::shinyDirButton(
              ns("output_folder_select"),
              "Selecionar pasta...",
              "Escolha a pasta de saída",
              class = "btn-outline-primary w-100"
            )
          ),
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
        shiny::div(
          class = "analysis-preview-map-frame",
          leaflet::leafletOutput(ns("preview_map"), height = "553px")
        )
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
    geo_source_is_local <- shiny::reactiveVal(FALSE)
    geo_data <- shiny::reactiveVal(NULL)
    geo_preview <- shiny::reactiveVal(NULL)
    geo_invalid_geometry <- shiny::reactiveVal(FALSE)
    analysis_result <- shiny::reactiveVal(NULL)
    validation_state <- shiny::reactiveVal(NULL)
    geo_validation_state <- shiny::reactiveVal(NULL)
    raster_validation_state <- shiny::reactiveVal(NULL)
    raster_inspection <- shiny::reactiveVal(NULL)
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
    geo_validation_preview_file <- shiny::reactiveVal(NULL)
    geo_validation_result_file <- shiny::reactiveVal(NULL)
    geo_validation_job_token <- shiny::reactiveVal(NULL)
    validated_geo_rds <- shiny::reactiveVal(NULL)
    running <- shiny::reactiveVal(FALSE)
    job_process <- shiny::reactiveVal(NULL)
    job_progress_file <- shiny::reactiveVal(NULL)
    job_result_file <- shiny::reactiveVal(NULL)
    progress_state <- shiny::reactiveVal(list(percent = 0, stage = "Aguardando", detail = "", status = "idle"))
    runtime_warnings <- shiny::reactiveVal(character())
    job_log_lines <- shiny::reactiveVal(character())
    extra_custom_class_ids <- shiny::reactiveVal(integer())
    next_extra_custom_class_id <- shiny::reactiveVal(0L)
    output_folder_roots <- c(
      "Home" = normalizePath(path.expand("~"), mustWork = FALSE),
      "Desktop" = file.path(normalizePath(path.expand("~"), mustWork = FALSE), "Desktop"),
      "Projeto" = normalizePath(getwd(), mustWork = FALSE)
    )
    if (dir.exists("/Volumes")) {
      output_folder_roots <- c(output_folder_roots, "Volumes" = "/Volumes")
    }
    output_folder_roots <- output_folder_roots[dir.exists(output_folder_roots)]
    if (!length(output_folder_roots)) {
      output_folder_roots <- c("Projeto" = normalizePath(getwd(), mustWork = FALSE))
    }

    shinyFiles::shinyFileChoose(
      input,
      "geo_local_select",
      roots = output_folder_roots,
      session = session,
      filetypes = unique(c("zip", supported_vector_extensions(), shapefile_extensions()))
    )

    shinyFiles::shinyDirChoose(
      input,
      "output_folder_select",
      roots = output_folder_roots,
      session = session,
      allowDirCreate = TRUE
    )

    shinyFiles::shinyDirChoose(
      input,
      "raster_local_select",
      roots = output_folder_roots,
      session = session,
      allowDirCreate = FALSE
    )

    shiny::observeEvent(input$output_folder_select, {
      selected_folder <- shinyFiles::parseDirPath(output_folder_roots, input$output_folder_select)
      if (length(selected_folder) && nzchar(selected_folder[[1]])) {
        shiny::updateTextInput(
          session,
          "output_folder",
          value = normalizePath(selected_folder[[1]], mustWork = FALSE)
        )
      }
    }, ignoreInit = TRUE)

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

    step_complete_badge <- function() {
      shiny::span(
        class = "step-complete-check",
        title = "Etapa concluída",
        shiny::icon("check")
      )
    }

    mesh_step_ready <- shiny::reactive({
      geospatial_ready() &&
        (
          isTRUE(input$no_mesh) ||
            (!is.null(input$mesh_size_km) && is.finite(input$mesh_size_km) && input$mesh_size_km > 0)
        )
    })

    raster_step_ready <- shiny::reactive({
      state <- raster_validation_state()
      !is.null(state) &&
        identical(state$status, "complete") &&
        nzchar(raster_folder_path()) &&
        !is.null(pixel_area_value()) &&
        is.finite(pixel_area_value()) &&
        pixel_area_value() > 0
    })

    output_step_ready <- shiny::reactive({
      !is.null(analysis_result())
    })

    output$step_geo_mesh_status <- shiny::renderUI({
      if (isTRUE(mesh_step_ready())) step_complete_badge()
    })

    output$step_rasters_status <- shiny::renderUI({
      if (isTRUE(raster_step_ready())) step_complete_badge()
    })

    output$step_output_status <- shiny::renderUI({
      if (isTRUE(output_step_ready())) step_complete_badge()
    })

    set_analysis_step <- function(value = character()) {
      try(bslib::accordion_panel_set("steps", value, session = session), silent = TRUE)
      invisible(NULL)
    }

    go_to_analysis_step <- function(value, delay = 0.35) {
      later::later(
        function() {
          set_analysis_step(value)
        },
        delay = delay
      )
      invisible(NULL)
    }

    close_analysis_steps <- function(delay = 0.35) {
      later::later(
        function() {
          set_analysis_step(character())
        },
        delay = delay
      )
      invisible(NULL)
    }

    mark_invalid_geospatial_file <- function(clear_preview = TRUE) {
      if (isTRUE(clear_preview)) {
        geo_path(NULL)
        geo_source_is_local(FALSE)
        geo_preview(NULL)
        shiny::updateSelectizeInput(session, "group_column", choices = NULL, selected = character(), server = TRUE)
      }
      geo_data(NULL)
      validated_geo_rds(NULL)
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
      raster_inspection(inspection)
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
        if (inspection$same_resolution) "resolução consistente" else "resoluções diferentes",
        if (isTRUE(inspection$grids_aligned)) {
          "grades alinhadas"
        } else {
          paste0(
            inspection$grid_group_count %||% inspection$count,
            " grades distintas; a malha será rasterizada separadamente"
          )
        }
      )
      validation_status <- if (inspection$same_crs && inspection$same_resolution) "complete" else "warning"
      raster_validation_state(list(
        status = validation_status,
        percent = 100,
        type = if (inspection$same_crs && inspection$same_resolution) "success" else "warning",
        text = paste0(
          "Validação concluída: ", inspection$count, " raster(s), anos ",
          inspection$years[[1]], "–", inspection$years[[2]], "; ",
          paste(consistency, collapse = "; "), ". Resolução: ",
          paste(format(inspection$resolution, digits = 7), collapse = " × "), "."
        )
      ))
      if (identical(validation_status, "complete")) {
        go_to_analysis_step("step_output")
      }
      invisible(inspection)
    }

    schedule_local_raster_validation <- function(folder, token) {
      stop_raster_validation_process()

      progress_file <- file.path(session_dir, paste0("raster_validation_", token, ".json"))
      result_file <- file.path(session_dir, paste0("raster_validation_", token, ".rds"))
      safe_unlink(c(progress_file, result_file))
      write_progress(progress_file, 1, "Validando pasta local", "Lendo rasters diretamente do caminho selecionado.", "running")

      process <- callr::r_bg(
        func = function(folder, progress_file, result_file, app_directory) {
          source(file.path(app_directory, "R", "utils.R"), local = globalenv())

          progress <- function(percent, stage, detail = NULL, status = "running") {
            write_progress(progress_file, percent, stage, detail, status)
          }

          tryCatch({
            inspection <- inspect_raster_folder(folder, progress = progress)
            saveRDS(
              list(
                staged_folder = normalizePath(folder, mustWork = TRUE),
                label = paste0("Pasta local: ", normalizePath(folder, mustWork = TRUE)),
                inspection = inspection
              ),
              result_file
            )
            progress(100, "Validação concluída", "Rasters locais validados.", "complete")
          }, error = function(e) {
            write_progress(progress_file, 100, "Erro", conditionMessage(e), "error")
            stop(e)
          })
        },
        args = list(
          folder = folder,
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
      geo_validation_preview_file(NULL)
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

    apply_geospatial_preview <- function(result, source_label = NULL) {
      preview <- result$preview %||% result$geo
      if (!inherits(preview, "sf") || !nrow(preview)) return(invisible(NULL))
      geo_preview(preview)
      columns <- result$columns %||% names(sf::st_drop_geometry(preview))
      selected_group_columns <- intersect(shiny::isolate(input$group_column) %||% character(), columns)
      update_mesh_recommendation_inputs(result$mesh_recommendation)
      shiny::updateSelectizeInput(
        session,
        "group_column",
        choices = stats::setNames(columns, columns),
        selected = selected_group_columns,
        server = TRUE
      )
      validation_state(list(
        type = "info",
        text = paste0(
          "Prévia do arquivo geoespacial carregada",
          if (!is.null(source_label) && nzchar(source_label)) paste0(": ", source_label) else "",
          ". Aguarde a validação completa para rodar a análise."
        )
      ))
      invisible(result)
    }

    apply_geospatial_validation <- function(result, source_label = NULL) {
      geo <- result$preview %||% result$geo
      if (!inherits(geo, "sf") || !nrow(geo)) {
        stop("A validação não retornou uma prévia geoespacial válida.", call. = FALSE)
      }
      geo_data(geo)
      geo_preview(geo)
      validated_path <- result$validated_geo_rds %||% NULL
      if (is.null(validated_path) || !length(validated_path) || !file.exists(validated_path[[1]])) {
        stop("A geometria validada não foi encontrada para executar a análise.", call. = FALSE)
      }
      validated_geo_rds(normalizePath(validated_path[[1]], mustWork = TRUE))
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
      go_to_analysis_step("step_rasters")
      invisible(result)
    }

    schedule_geospatial_validation <- function(path, source_label, token) {
      stop_geospatial_validation_process()

      progress_file <- file.path(session_dir, paste0("geo_validation_", token, ".json"))
      preview_file <- file.path(session_dir, paste0("geo_preview_", token, ".rds"))
      result_file <- file.path(session_dir, paste0("geo_validation_", token, ".rds"))
      validated_file <- file.path(session_dir, paste0("geo_validated_", token, ".rds"))
      safe_unlink(c(progress_file, preview_file, result_file, validated_file))
      write_progress(progress_file, 2, "Leitura do arquivo", "Abrindo geometria em segundo plano.", "running")

      process <- callr::r_bg(
        func = function(path, source_label, progress_file, preview_file, result_file, validated_file, app_directory) {
          source(file.path(app_directory, "R", "utils.R"), local = globalenv())
          source(file.path(app_directory, "R", "spatial_io.R"), local = globalenv())

          progress <- function(percent, stage, detail = NULL, status = "running") {
            write_progress(progress_file, percent, stage, detail, status)
          }

          tryCatch({
            progress(3, "Leitura do arquivo", "Lendo feições e atributos uma única vez.")
            raw_geo <- sf::st_read(path, quiet = TRUE, stringsAsFactors = FALSE)
            if (!inherits(raw_geo, "sf") || !nrow(raw_geo)) {
              stop("O arquivo geoespacial não contém feições.", call. = FALSE)
            }

            progress(10, "Preparando prévia", "Simplificando a geometria para o mapa interativo.")
            preview <- preview_from_loaded_geo(raw_geo, tolerance_fraction = 0.001)
            preview_recommendation <- recommend_mesh_size_km(
              preview,
              target_cells = 1000L,
              max_cells = 20000L
            )
            save_rds_atomic(
              list(
                preview = preview,
                columns = names(sf::st_drop_geometry(preview)),
                mesh_recommendation = preview_recommendation,
                source_label = source_label
              ),
              preview_file
            )
            progress(20, "Prévia pronta", "Mapa liberado; validando a geometria completa em segundo plano.")

            validation_progress <- function(percent, stage, detail = NULL, status = "running") {
              scaled <- 20 + (max(0, min(100, as.numeric(percent))) / 100) * 65
              progress(scaled, stage, detail, status)
            }
            geo <- read_geo(
              raw_geo,
              progress = validation_progress,
              reject_invalid = identical(tolower(tools::file_ext(path)), "shp")
            )
            progress(88, "Estimativa da malha", "Calculando sugestão pela extensão do arquivo.")
            mesh_recommendation <- recommend_mesh_size_km(
              geo,
              target_cells = 1000L,
              max_cells = 20000L
            )
            progress(94, "Resumo do arquivo", "Preparando atributos e resumo geoespacial.")
            save_rds_atomic(list(geo = geo), validated_file)
            save_rds_atomic(
              list(
                preview = preview,
                columns = names(sf::st_drop_geometry(geo)),
                mesh_recommendation = mesh_recommendation,
                source_label = source_label,
                validated_geo_rds = normalizePath(validated_file, mustWork = TRUE)
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
            preview_file = preview_file,
            result_file = result_file,
            validated_file = validated_file,
            app_directory = app_root()
        ),
        supervise = TRUE,
        stdout = "|",
        stderr = "|"
      )

      geo_validation_process(process)
      geo_validation_progress_file(progress_file)
      geo_validation_preview_file(preview_file)
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
      set_geo_validation_progress(2, "Leitura do arquivo", "Abrindo arquivo em segundo plano.")
      preview <- tryCatch(read_gpkg_bbox_preview(path), error = function(e) NULL)
      if (!is.null(preview)) geo_preview(preview)
      set_geo_validation_progress(
        3,
        "Leitura do arquivo",
        if (is.null(preview)) "Preparando a prévia e a validação." else "Extensão carregada; preparando a geometria simplificada."
      )
      validation_state(list(
        type = "info",
        text = paste0(
          "Arquivo geoespacial recebido",
          if (!is.null(source_label) && nzchar(source_label)) paste0(": ", source_label) else "",
          ". Aguarde a preparação da prévia e a validação completa para rodar a análise."
        )
      ))
      schedule_geospatial_validation(path, source_label, token)
    }

    shiny::observeEvent(input$raster_local_select, {
      selected_folder <- shinyFiles::parseDirPath(output_folder_roots, input$raster_local_select)
      if (!length(selected_folder) || !nzchar(selected_folder[[1]]) || !dir.exists(selected_folder[[1]])) {
        return(NULL)
      }
      token <- raster_validation_token() + 1L
      raster_validation_token(token)
      folder <- normalizePath(selected_folder[[1]], mustWork = TRUE)
      raster_folder_path(folder)
      raster_folder_label(paste0("Pasta local: ", folder))
      validation_state(NULL)
      raster_inspection(NULL)
      pixel_area_value(NULL)
      pixel_area_summary("—")
      toggle_run_button(TRUE)
      set_raster_validation_progress(
        1,
        "Validando pasta local",
        "Rasters serão lidos diretamente do caminho selecionado."
      )
      schedule_local_raster_validation(folder, token)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$clear_raster_folder, {
      raster_validation_token(raster_validation_token() + 1L)
      stop_raster_validation_process()
      raster_folder_path("")
      raster_folder_label("")
      validation_state(NULL)
      raster_validation_state(NULL)
      raster_inspection(NULL)
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

    shiny::observeEvent(input$geo_local_select, {
      selected <- shinyFiles::parseFilePaths(output_folder_roots, input$geo_local_select)
      if (is.null(selected) || !nrow(selected)) return(NULL)
      selected_path <- selected$datapath[[1]]
      if (is.null(selected_path) || !nzchar(selected_path) || !file.exists(selected_path)) return(NULL)

      token <- geo_validation_token() + 1L
      geo_validation_token(token)
      stop_geospatial_validation_process()
      geo_path(NULL)
      geo_source_is_local(!identical(tolower(tools::file_ext(selected_path)), "zip"))
      geo_data(NULL)
      geo_preview(NULL)
      validated_geo_rds(NULL)
      geo_invalid_geometry(FALSE)
      validation_state(NULL)
      set_geo_validation_progress(1, "Recebendo arquivo", "Preparando arquivo local selecionado.")
      toggle_run_button(TRUE)

      source_label <- basename(selected_path)
      later::later(function() {
        if (!identical(token, shiny::isolate(geo_validation_token()))) return(NULL)
        tryCatch({
          set_geo_validation_progress(2, "Preparando arquivo", "Buscando arquivos auxiliares no mesmo diretório.")
          path <- if (tolower(tools::file_ext(selected_path)) == "zip") {
            upload_dir <- file.path(session_dir, paste0("geo_", as.integer(Sys.time())))
            stage_local_vector(selected_path, upload_dir)
          } else {
            normalizePath(selected_path, mustWork = TRUE)
          }
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

    shiny::observeEvent(input$geo_upload, {
      upload <- input$geo_upload
      if (is.null(upload) || !nrow(upload)) return(NULL)
      token <- geo_validation_token() + 1L
      geo_validation_token(token)
      stop_geospatial_validation_process()
      geo_path(NULL)
      geo_source_is_local(FALSE)
      geo_data(NULL)
      geo_preview(NULL)
      validated_geo_rds(NULL)
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
      if (isTRUE(input$no_mesh)) return(NULL)

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
      create.preview.mesh(
        geo,
        mesh.size = size_km * 1000,
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
      leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satélite (Esri)",
          options = leaflet::providerTileOptions(maxZoom = 20)
        ) |>
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "Ruas") |>
        leaflet::hideGroup("Ruas") |>
        leaflet::addMapPane("meshPane", zIndex = 410) |>
        leaflet::addMapPane("studyPane", zIndex = 420) |>
        leaflet::addScaleBar(
          position = "bottomleft",
          options = leaflet::scaleBarOptions(
            maxWidth = 160,
            metric = TRUE,
            imperial = FALSE,
            updateWhenIdle = TRUE
          )
        ) |>
        leaflet::addControl(
          html = paste0(
            "<div class='preview-north-arrow' aria-label='Norte'>",
            "<div class='preview-north-label'>N</div>",
            "<div class='preview-north-pointer'>▲</div>",
            "</div>"
          ),
          position = "topright",
          className = "preview-north-control"
        ) |>
        leaflet::addLayersControl(
          baseGroups = c("Satélite (Esri)", "Ruas"),
          overlayGroups = c("Prévia do limite", "Malha"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::setView(lng = -54, lat = -12, zoom = 4)
    })

    shiny::observe({
      geo <- geo_preview() %||% geo_data()
      proxy <- leaflet::leafletProxy("preview_map", session = session) |>
        leaflet::clearGroup("Prévia do limite")
      if (is.null(geo)) {
        proxy |> leaflet::setView(lng = -54, lat = -12, zoom = 4)
        return(invisible(NULL))
      }

      bbox <- sf::st_bbox(geo)
      proxy |>
        leaflet::addPolygons(
          data = geo,
          fill = TRUE,
          fillColor = "#000000",
          fillOpacity = 0.6,
          color = "transparent",
          weight = 0,
          opacity = 0,
          group = "Prévia do limite",
          options = leaflet::pathOptions(pane = "studyPane")
        ) |>
        leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    })

    shiny::observe({
      geo <- geo_preview() %||% geo_data()
      proxy <- leaflet::leafletProxy("preview_map", session = session) |>
        leaflet::clearGroup("Malha")
      if (is.null(geo) || isTRUE(input$no_mesh)) return(invisible(NULL))

      mesh <- tryCatch(preview_mesh(), error = function(e) NULL)
      if (is.null(mesh) || !nrow(mesh)) return(invisible(NULL))
      proxy |>
        leaflet::addPolygons(
          data = mesh_for_leaflet(mesh),
          fillColor = "#F6C344",
          fillOpacity = 0.08,
          color = "#FFD54F",
          weight = 0.7,
          opacity = 0.9,
          group = "Malha",
          options = leaflet::pathOptions(pane = "meshPane")
        )
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
      preview_file <- geo_validation_preview_file()
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

      if (!is.null(preview_file) && file.exists(preview_file)) {
        preview_result <- tryCatch(readRDS(preview_file), error = function(e) NULL)
        if (!is.null(preview_result)) {
          geo_validation_preview_file(NULL)
          safe_unlink(preview_file)
          apply_geospatial_preview(
            preview_result,
            preview_result$source_label %||% NULL
          )
        }
      }

      if (!process$is_alive()) {
        exit_status <- process$get_exit_status()
        geo_validation_process(NULL)
        geo_validation_progress_file(NULL)
        geo_validation_preview_file(NULL)
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
          validated_geo_rds(NULL)
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
          raster_inspection(NULL)
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
      inspection <- raster_inspection()
      rasters <- inspection$index
      if (!is.data.frame(rasters) || !nrow(rasters) || !all(file.exists(rasters$path))) {
        rasters <- list_raster_files(raster_folder_path())
      }
      output_folder <- ensure_output_folder(input$output_folder)
      if (is.null(pixel_area_value()) || !is.finite(pixel_area_value()) || pixel_area_value() <= 0) {
        stop("Selecione uma pasta de rasters válida para estimar a área do pixel.", call. = FALSE)
      }
      if (is.null(input$mapbiomas) || !nzchar(input$mapbiomas)) {
        stop("Selecione uma legenda das classes antes de rodar o algoritmo.", call. = FALSE)
      }
      mapbiomas_class_map(input$mapbiomas, custom_classes())
      list(
        rasters = rasters,
        inspection = inspection,
        output_folder = output_folder,
        resume_proxy = FALSE
      )
    }

    append_runtime_warning <- function(text, notify = TRUE) {
      text <- trimws(gsub("\033\\[[0-9;]*[[:alpha:]]", "", as.character(text %||% "")))
      if (!nzchar(text)) return(invisible(NULL))
      current <- runtime_warnings()
      if (text %in% current) return(invisible(NULL))
      runtime_warnings(c(current, text))
      if (isTRUE(notify)) {
        shiny::showNotification(text, type = "warning", duration = 12)
      }
      invisible(text)
    }

    collect_job_output <- function(process) {
      output_lines <- tryCatch(process$read_output_lines(), error = function(e) character())
      error_lines <- tryCatch(process$read_error_lines(), error = function(e) character())
      new_lines <- c(output_lines, error_lines)
      if (!length(new_lines)) return(invisible(character()))

      accumulated <- utils::tail(c(job_log_lines(), new_lines), 500L)
      job_log_lines(accumulated)
      disk_patterns <- paste(
        c(
          "estimated disk space needed",
          "insufficient disk space",
          "disk available",
          "disk needed",
          "out of disk space",
          "disk is full",
          "disco.*(insuficiente|disponível|necessário)"
        ),
        collapse = "|"
      )
      hits <- grep(disk_patterns, new_lines, ignore.case = TRUE)
      if (length(hits)) {
        for (index in hits) {
          warning_lines <- new_lines[index:min(length(new_lines), index + 1L)]
          append_runtime_warning(paste(trimws(warning_lines), collapse = " "))
        }
      }
      invisible(new_lines)
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
      warnings <- runtime_warnings()
      shiny::tagList(
        if (!is.null(message)) {
          app_alert(message$text, color = message$type, dismissible = TRUE)
        },
        lapply(warnings, function(text) {
          app_alert(text, color = "warning", dismissible = TRUE)
        })
      )
    })

    shiny::observeEvent(input$run, {
      if (running()) return()
      tryCatch({
        runtime_warnings(character())
        job_log_lines(character())
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
          raster_index = if (resume_proxy) NULL else validated$rasters,
          validated_geo_rds = if (resume_proxy) NULL else validated_geo_rds(),
          max_cells = 1000000L,
          use_direct_geo_path = !resume_proxy && isTRUE(geo_source_is_local()),
          use_direct_raster_path = !resume_proxy,
          use_direct_paths = !resume_proxy &&
            isTRUE(geo_source_is_local()),
          resume_proxy = resume_proxy
        )

        if (!resume_proxy) {
          advisory <- disk_space_advisory(validated$inspection, validated$output_folder)
          if (!is.null(advisory)) append_runtime_warning(advisory$text)
        }

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

      collect_job_output(process)
      progress_state(read_progress(job_progress_file()))
      if (!process$is_alive()) {
        exit_status <- process$get_exit_status()
        running(FALSE)
        toggle_run_button(FALSE)

        if (identical(exit_status, 0L) && file.exists(job_result_file())) {
          result <- read_analysis_job_result(job_result_file(), lazy = TRUE)
          analysis_result(result)
          progress_state(list(
            percent = 100,
            stage = "Concluído",
            detail = "Análise finalizada e arquivos exportados.",
            status = "complete"
          ))
          validation_state(list(type = "success", text = "Análise concluída com sucesso."))
          runtime_warnings(character())
          shiny::showNotification("Análise concluída com sucesso.", type = "message", duration = 8)
          close_analysis_steps()
        } else {
          collect_job_output(process)
          error_lines <- job_log_lines()
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
      available_levels <- analysis_result_levels(result)
      shiny::req(length(available_levels))
      level <- input$result_level
      if (is.null(level) || length(level) != 1L || !nzchar(level)) {
        level <- if ("grid" %in% available_levels) "grid" else available_levels[[1]]
      }
      if (!level %in% available_levels) {
        level <- if ("grid" %in% available_levels) "grid" else available_levels[[1]]
      }
      load_analysis_result_level(result, level)
    })

    shiny::observeEvent(analysis_result(), {
      result <- analysis_result()
      if (is.null(result)) return()
      levels <- analysis_result_levels(result)
      if (!length(levels)) return()
      labels <- analysis_level_labels()[levels]
      selected <- if ("grid" %in% levels) "grid" else levels[[1]]
      shiny::updateSelectInput(session, "result_level", choices = labels, selected = selected)
    }, ignoreNULL = TRUE)

    output$result_summary <- shiny::renderUI({
      result <- analysis_result()
      shiny::req(result)
      group_columns <- normalize_group_columns(result$group_column %||% character(), max_columns = 2L)
      group_count <- result$group_count %||% 1L
      if (!is_lazy_analysis_result(result) && !is.null(result$groups) &&
          length(group_columns) && all(group_columns %in% names(result$groups))) {
        group_count <- nrow(unique(sf::st_drop_geometry(result$groups)[group_columns]))
      }
      year_min <- if (is_lazy_analysis_result(result)) {
        result$year_min
      } else if (!is.null(result$raster_index$year) && length(result$raster_index$year)) {
        min(result$raster_index$year, na.rm = TRUE)
      } else {
        NA
      }
      year_max <- if (is_lazy_analysis_result(result)) {
        result$year_max
      } else if (!is.null(result$raster_index$year) && length(result$raster_index$year)) {
        max(result$raster_index$year, na.rm = TRUE)
      } else {
        NA
      }
      mesh_count <- if (is_lazy_analysis_result(result)) {
        result$mesh_count %||% 0
      } else {
        if (is.null(result$mesh)) 0 else nrow(result$mesh)
      }
      raster_count <- if (is_lazy_analysis_result(result)) {
        result$raster_count %||% 0
      } else {
        if (is.null(result$raster_index)) 0 else nrow(result$raster_index)
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
            format(mesh_count, big.mark = ".", decimal.mark = ","),
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
          bslib::value_box("Rasters", raster_count, showcase = shiny::icon("images"), theme = "warning")
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
          pageLength = 5,
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
