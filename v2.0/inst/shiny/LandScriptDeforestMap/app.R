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
         });
         (function enableRasterDirectoryUpload() {
           var rasterPattern = /\\.(tif|tiff|img|vrt|grd|zip)$/i;

           function applyRasterDirectoryAttributes() {
             var inputs = document.querySelectorAll('input[type=\"file\"][id$=\"raster_upload\"]');
             inputs.forEach(function(input) {
               input.setAttribute('multiple', 'multiple');
               input.setAttribute('webkitdirectory', '');
               input.setAttribute('directory', '');
             });
           }

           function readAllDirectoryEntries(reader) {
             var entries = [];
             return new Promise(function(resolve, reject) {
               function readBatch() {
                 reader.readEntries(function(batch) {
                   if (!batch.length) {
                     resolve(entries);
                   } else {
                     entries = entries.concat(Array.prototype.slice.call(batch));
                     readBatch();
                   }
                 }, reject);
               }
               readBatch();
             });
           }

           function traverseEntry(entry) {
             return new Promise(function(resolve, reject) {
               if (entry.isFile) {
                 entry.file(function(file) {
                   resolve(rasterPattern.test(file.name) ? [file] : []);
                 }, reject);
                 return;
               }
               if (entry.isDirectory) {
                 readAllDirectoryEntries(entry.createReader())
                   .then(function(entries) {
                     return Promise.all(entries.map(traverseEntry));
                   })
                   .then(function(groups) {
                     resolve([].concat.apply([], groups));
                   })
                   .catch(reject);
                 return;
               }
               resolve([]);
             });
           }

           function collectDroppedRasterFiles(event) {
             var items = event.dataTransfer && event.dataTransfer.items;
             if (items && items.length) {
               var jobs = Array.prototype.slice.call(items).map(function(item) {
                 var entry = item.webkitGetAsEntry ? item.webkitGetAsEntry() : null;
                 if (entry) return traverseEntry(entry);
                 var file = item.getAsFile ? item.getAsFile() : null;
                 return Promise.resolve(file && rasterPattern.test(file.name) ? [file] : []);
               });
               return Promise.all(jobs).then(function(groups) {
                 return [].concat.apply([], groups);
               });
             }
             var files = event.dataTransfer && event.dataTransfer.files ?
               Array.prototype.slice.call(event.dataTransfer.files) : [];
             return Promise.resolve(files.filter(function(file) {
               return rasterPattern.test(file.name);
             }));
           }

           function setFilesOnInput(input, files) {
             if (!files.length) {
               window.alert('Nenhum raster suportado foi encontrado. Use .tif, .tiff, .img, .vrt, .grd ou .zip.');
               return;
             }
             if (typeof DataTransfer === 'undefined') {
               window.alert('Seu navegador não permitiu arrastar a pasta. Use o botão para selecionar a pasta.');
               return;
             }
             var transfer = new DataTransfer();
             files.forEach(function(file) {
               transfer.items.add(file);
             });
             input.files = transfer.files;
             input.dispatchEvent(new Event('change', { bubbles: true }));
           }

           function enableRasterDropzones() {
             document.querySelectorAll('.raster-upload-dropzone').forEach(function(zone) {
               if (zone.dataset.landScriptDropEnabled === 'true') return;
               zone.dataset.landScriptDropEnabled = 'true';
               var input = zone.querySelector('input[type=\"file\"][id$=\"raster_upload\"]');
               if (!input) return;

               ['dragenter', 'dragover'].forEach(function(name) {
                 zone.addEventListener(name, function(event) {
                   event.preventDefault();
                   event.stopPropagation();
                   zone.classList.add('drag-active');
                 });
               });
               ['dragleave', 'drop'].forEach(function(name) {
                 zone.addEventListener(name, function(event) {
                   event.preventDefault();
                   event.stopPropagation();
                   zone.classList.remove('drag-active');
                 });
               });
               zone.addEventListener('drop', function(event) {
                 collectDroppedRasterFiles(event)
                   .then(function(files) {
                     setFilesOnInput(input, files);
                   })
                   .catch(function(error) {
                     window.alert('Não foi possível ler a pasta arrastada: ' + error.message);
                   });
               });
             });
           }

           function initRasterUploadEnhancements() {
             applyRasterDirectoryAttributes();
             enableRasterDropzones();
           }

           if (document.readyState === 'loading') {
             document.addEventListener('DOMContentLoaded', initRasterUploadEnhancements);
           } else {
             initRasterUploadEnhancements();
           }
           window.setTimeout(initRasterUploadEnhancements, 250);
           window.setTimeout(initRasterUploadEnhancements, 1000);
           if (window.MutationObserver) {
             var rasterObserverTimer = null;
             new MutationObserver(function() {
               window.clearTimeout(rasterObserverTimer);
               rasterObserverTimer = window.setTimeout(initRasterUploadEnhancements, 80);
             }).observe(document.body, {
               childList: true,
               subtree: true
             });
           }
         })();"
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
