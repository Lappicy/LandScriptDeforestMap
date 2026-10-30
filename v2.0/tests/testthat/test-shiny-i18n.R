test_that("interface and generated messages have English translations", {
  expect_identical(ui_translate(c(
    "Executar análise", "Gráficos e mapas", "Tutorial e como citar",
    "1. Arquivo geoespacial e malha", "2. Rasters e classes",
    "3. Saída e execução", "Como citar", "Apoio institucional"
  ), "en"), c(
    "Run analysis", "Charts and maps", "Tutorial and citation",
    "1. Geospatial file and grid", "2. Rasters and classes",
    "3. Output and execution", "How to cite", "Institutional support"
  ))
  messages <- c(
    "Validação concluída: 2 raster(s), anos 2020–2021; CRS consistente.",
    "Raster 1 de 2", "Usando arquivo manual: soil_2020.gpkg — camada grid, 2 registros com geometria",
    "Classe adicional 2", "Nível: Malha (ID_mesh)",
    "A pasta de rasters não existe: /tmp/rasters"
  )
  expect_identical(ui_translate(messages, "en"), c(
    "Validation completed: 2 raster(s), years 2020–2021; Consistent CRS.",
    "Raster 1 of 2", "Using uploaded file: soil_2020.gpkg — layer grid, 2 records with geometry",
    "Additional class 2", "Level: Grid (ID_mesh)",
    "The raster folder does not exist: /tmp/rasters"
  ))
  expect_identical(ui_translate(messages, "pt-BR"), messages)
  expect_identical(ui_translate(c("Bare Soil", "Rock & sand $", "Soil_type", NA), "en"),
                   c("Bare Soil", "Rock & sand $", "Soil_type", NA_character_))
  expect_identical(ui_translate("  Nome da análise  ", "en"), "  Analysis name  ")
  expect_identical(datatable_language("en")$search, "Search:")
  expect_identical(datatable_language("pt-BR")$paginate$`next`, "Próxima")
})

test_that("French covers the interface and dynamic messages without changing supplied names", {
  expect_setequal(names(app_translations$fr), names(app_translations$text))
  expect_identical(ui_translate(c(
    "Tutorial e como citar",
    "Análise de mudanças espaciais e temporais em imagens classificadas",
    "Raster 1 de 2", "Nível: Malha (ID_mesh)",
    "Usando arquivo manual: soil_2020.gpkg — camada grid, 2 registros com geometria",
    "Classe adicional 2"
  ), "fr"), c(
    "Tutoriel et citation",
    "Analyse des changements spatiaux et temporels dans les images classifiées",
    "Raster 1 sur 2", "Niveau : Grille (ID_mesh)",
    "Fichier importé utilisé : soil_2020.gpkg — couche grid, 2 enregistrements avec géométrie",
    "Classe supplémentaire 2"
  ))
  expect_identical(ui_translate(c("Bare Soil", "Soil_type", NA), "fr"),
                   c("Bare Soil", "Soil_type", NA_character_))
  expect_identical(datatable_language("fr")$search, "Rechercher :")
  expect_identical(datatable_language("fr")$paginate$`next`, "Suivante")
})

test_that("shared language updates plot defaults and preserves custom inputs", {
  source(file.path(landscript_test_app_dir, "R", "mod_visuals.R"), local = TRUE)
  language <- shiny::reactiveVal("pt-BR")
  shiny::testServer(visuals_server, args = list(
    automatic_result = shiny::reactive(NULL), language = language
  ), {
    session$setInputs(chart_title = "Desmatamento e variação das classes",
                      map_title = "My custom map", chart_width = 310,
                      primary_column = "Água / solo (50%)")
    language("en")
    session$flushReact()
    expect_identical(chart_title_value(), "Deforestation and class variation")
    expect_identical(input$map_title, "My custom map")
    expect_equal(input$chart_width, 310)
    expect_identical(input$primary_column, "Água / solo (50%)")
    language("fr")
    session$flushReact()
    expect_identical(chart_title_value(), "Déforestation et variation des classes")
    expect_identical(input$map_title, "My custom map")
    expect_identical(input$primary_column, "Água / solo (50%)")

    csv <- tempfile(fileext = ".csv")
    writeLines(c(
      "Year,Floresta,Variation_Soil_type,Grupos",
      "2020,10,1,Norte", "2021,12,2,Norte", "2022,15,3,Sul"
    ), csv)
    session$setInputs(manual_result = data.frame(
      name = "classes.csv", datapath = csv,
      size = file.info(csv)$size, type = "text/csv"
    ))
    expected_columns <- c("Year", "Floresta", "Variation_Soil_type", "Grupos")
    expect_identical(names(current_table()), expected_columns)
    language("pt-BR")
    session$flushReact()
    expect_identical(names(current_table()), expected_columns)
    language("en")
    session$flushReact()
    expect_identical(names(current_table()), expected_columns)
    expect_identical(current_table()$Grupos, c("Norte", "Norte", "Sul"))
    language("fr")
    session$flushReact()
    expect_identical(names(current_table()), expected_columns)
    expect_identical(current_table()$Grupos, c("Norte", "Norte", "Sul"))
    unlink(csv)

    session$setInputs(chart_title = "Meu título personalizado")
    language("pt-BR")
    session$flushReact()
    expect_identical(chart_title_value(), "Meu título personalizado")
    language("fr")
    session$flushReact()
    expect_identical(chart_title_value(), "Meu título personalizado")
    language("en")
    session$flushReact()
    expect_identical(chart_title_value(), "Meu título personalizado")
  })
})

test_that("navigation menu controls a language belonging to each session", {
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(landscript_test_app_dir)
  app <- source("app.R", local = new.env(parent = globalenv()))$value
  shiny::testServer(app, {
    session$flushReact()
    expect_identical(language(), "pt-BR")
    expect_match(output$language_menu$html, "Language / Idioma", fixed = TRUE)
    session$setInputs(language_en = 1)
    expect_identical(language(), "en")
    english_menu <- output$language_menu$html
    expect_match(english_menu, 'data-bs-toggle="dropdown"', fixed = TRUE)
    expect_match(english_menu, 'id="language_en"', fixed = TRUE)
    expect_match(english_menu, 'aria-current="true"', fixed = TRUE)
    expect_false(grepl("Language / Idioma", english_menu, fixed = TRUE))
    session$setInputs(language_fr = 1)
    expect_identical(language(), "fr")
    expect_match(output$language_menu$html, "Language / Langue", fixed = TRUE)
    expect_match(output$language_menu$html, "landscript/flags/fr.svg", fixed = TRUE)
    session$setInputs(language_pt = 1)
    expect_identical(language(), "pt-BR")
    expect_match(output$language_menu$html, "Language / Idioma", fixed = TRUE)
  })
  shiny::testServer(app, {
    session$flushReact()
    expect_identical(language(), "pt-BR")
  })
})

test_that("French chart and map labels retain user columns and use French defaults", {
  data <- data.frame(Year = 2020:2022, Deforestation = c(2, 4, 3), check.names = FALSE)
  data[["Variation_Rock & sand $"]] <- c(1, 2, 3)
  original_names <- names(data)
  chart <- build_timeseries_plot(data, comparison.columns = "Variation_Rock & sand $", language = "fr")
  expect_identical(chart$labels$title, "Déforestation et variation des classes")
  expect_identical(chart$labels$x, "Année")
  expect_identical(chart$scales$get_scales("colour")$labels,
                   c("Déforestation", "Variation de Rock & sand $"))
  expect_silent(ggplot2::ggplot_build(chart))
  polygon <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  spatial <- sf::st_sf(data, geometry = sf::st_sfc(rep(list(polygon), 3), crs = 4326))
  map <- mesh.map(spatial, class = "Deforestation", year.used = 2021, language = "fr")
  expect_identical(map$labels$title, "Déforestation en 2021")
  expect_identical(map$labels$fill, "Déforestation (km²)")
  expect_identical(map$scales$get_scales("fill")$breaks[[1]], "Sans Déforestation")
  expect_silent(ggplot2::ggplot_build(map))
  expect_identical(names(data), original_names)
})
