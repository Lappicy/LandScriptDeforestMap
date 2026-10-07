app_directory <- normalizePath(getwd(), mustWork = TRUE)
options(
  landscript.app_dir = app_directory,
  shiny.maxRequestSize = 20 * 1024^3,
  scipen = 9999
)

local_library <- file.path(app_directory, ".Rlib")
if (dir.exists(local_library)) {
  .libPaths(c(local_library, .libPaths()))
}

source(file.path(app_directory, "R", "utils.R"), local = globalenv())
check_required_packages()

source(file.path(app_directory, "R", "spatial_io.R"), local = globalenv())
source(file.path(app_directory, "R", "landscript_engine.R"), local = globalenv())
source(file.path(app_directory, "R", "plots.R"), local = globalenv())
source(file.path(app_directory, "R", "mod_analysis.R"), local = globalenv())
source(file.path(app_directory, "R", "mod_visuals.R"), local = globalenv())

patent_certificate_href <- "landscript/INPI.pdf"

theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#176B5B",
  secondary = "#57706B",
  success = "#2C8C65",
  info = "#3686A0",
  warning = "#D39B2A",
  danger = "#B74747",
  base_font = "system-ui",
  heading_font = "system-ui"
)

citation_box <- function() {
  shiny::div(
    class = "citation-footer",
    bslib::card(
      fill = FALSE,
      class = "citation-card",
      bslib::card_header(
        shiny::span(shiny::icon("quote-left"), " Como citar")
      ),
      shiny::p(
        "O LandScriptDeforestMap pode ser utilizado livremente conforme a licença do projeto. ",
        "Ao utilizar o software, os resultados ou esta plataforma, cite o artigo completo:"
      ),
      shiny::tags$blockquote(
        class = "citation-text",
        "Lappicy, T.; Cabral, A. I. R.; Da Silva, R. G. P.; Arguelho, J. S.; ",
        "de Andrade, S. P. B.; Pereira, A. K.; Laques, A.-E.; Saito, C. H. (2024). ",
        shiny::em("LandScriptDeforestMap: An R package to evaluate deforestation in remote sensing images. "),
        "SoftwareX, 27, 101799. ",
        shiny::a(
          "https://doi.org/10.1016/j.softx.2024.101799",
          href = "https://doi.org/10.1016/j.softx.2024.101799",
          target = "_blank",
          rel = "noopener noreferrer"
        )
      ),
      shiny::p(
        shiny::strong("Registro no INPI: "),
        "Certificado de Registro de Programa de Computador LandScriptDeforestMap, ",
        "processo BR512024004176-1, publicado em 29/06/2024 e expedido em 12/11/2024. ",
        "Titulares: Thiago Lappicy Lemos Gomes, Carlos Hiroo Saito e Romero Gomes Pereira da Silva. ",
        shiny::a(
          "Consultar certificado",
          href = patent_certificate_href,
          target = "_blank",
          rel = "noopener noreferrer"
        ),
        "."
      ),
      shiny::p(
        class = "citation-reminder",
        shiny::icon("circle-info"),
        " O uso é aberto, mas a citação reconhece o trabalho científico e o registro do software."
      )
    )
  )
}

logo_footer <- function() {
  shiny::div(
    class = "institutional-footer",
    shiny::div(
      class = "institutional-footer-inner",
      shiny::span(class = "institutional-footer-label", "Apoio institucional"),
      shiny::div(
        class = "institutional-logo-row",
        shiny::img(src = "landscript/logos/unb.png", alt = "Universidade de Brasília (UnB)", class = "institutional-logo logo-unb"),
        shiny::img(src = "landscript/logos/progysat.png", alt = "PROGYSAT", class = "institutional-logo logo-progysat"),
        shiny::img(src = "landscript/logos/odisseia-inct.png", alt = "Odisseia INCT", class = "institutional-logo logo-odisseia")
      )
    )
  )
}

app_footer <- function() {
  shiny::tagList(
    citation_box(),
    logo_footer()
  )
}

testing_notice <- function() {
  shiny::div(
    class = "testing-notice",
    shiny::span(
      shiny::icon("flask"),
      " Este dashboard está em fase de testes. Para reportar erros, bugs, sugestões ou dúvidas, responda o ",
      shiny::a(
        "formulário",
        href = "https://forms.gle/SdzYLxZKnSYvWKnY7",
        target = "_blank",
        rel = "noopener noreferrer"
      ),
      " ou envie e-mail para ",
      shiny::a("lappicy@gmail.com", href = "mailto:lappicy@gmail.com"),
      "."
    )
  )
}

ui <- bslib::page_navbar(
  title = shiny::div(
    class = "brand-lockup",
    shiny::span(class = "brand-mark", shiny::icon("earth-americas")),
    shiny::strong("LandScriptDeforestMap")
  ),
  theme = theme,
  fillable = FALSE,
  window_title = "LandScriptDeforestMap — Plataforma Shiny",
  footer = app_footer(),
  header = shiny::tagList(
    shiny::useBusyIndicators(
      spinners = TRUE,
      pulse = FALSE,
      fade = TRUE
    ),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      shiny::tags$script(shiny::HTML(
        "Shiny.addCustomMessageHandler('toggle-disabled', function(x) {
           var el = document.getElementById(x.id);
           if (el) {
             el.disabled = x.disabled;
             el.setAttribute('aria-disabled', x.disabled ? 'true' : 'false');
           }
         });
         Shiny.addCustomMessageHandler('landscript-reset', function(x) {
           window.location.reload();
         });"
      ))
    ),
    testing_notice()
  ),
  bslib::nav_panel(
    "Executar análise",
    icon = shiny::icon("gears"),
    analysis_ui("analysis")
  ),
  bslib::nav_panel(
    "Gráficos e mapas",
    icon = shiny::icon("chart-line"),
    visuals_ui("visuals")
  ),
  bslib::nav_panel(
    "Sobre e Como citar",
    icon = shiny::icon("circle-info"),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Sobre a plataforma"),
        shiny::h3("Análise espacial de desmatamento com imagens classificadas"),
        shiny::p(
          "Esta aplicação integra o fluxo do LandScriptDeforestMap em uma interface ",
          "interativa: criação de malha, contagem zonal das classes, conversão para ",
          "área, variações anuais, agregações por limites, gráficos e mapas."
        ),
        shiny::p(
          "Os cálculos são executados localmente. Nenhum arquivo geoespacial ou raster ",
          "é enviado para serviços externos."
        )
      ),
      bslib::card(
        bslib::card_header("Fluxo"),
        shiny::tags$ol(
          shiny::tags$li("Carregue o limite geoespacial."),
          shiny::tags$li("Defina grupos e tamanho da malha."),
          shiny::tags$li("Informe os rasters e a legenda."),
          shiny::tags$li("Execute e acompanhe o progresso."),
          shiny::tags$li("Explore e baixe tabelas, mapas e gráficos.")
        )
      )
    )
  ),
  bslib::nav_item(
    shiny::actionButton(
      "reset_app",
      "Resetar",
      icon = shiny::icon("rotate-left"),
      class = "btn-outline-light btn-sm reset-app-button"
    )
  )
)

server <- function(input, output, session) {
  shiny::observeEvent(input$reset_app, {
    session$sendCustomMessage("landscript-reset", list())
  }, ignoreInit = TRUE)

  result <- analysis_server("analysis")
  visuals_server("visuals", result)
}

shiny::shinyApp(ui, server)
