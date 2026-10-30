# Session language is passed explicitly to modules; never store it in options().
app_translations <- jsonlite::read_json(
  file.path(getOption("landscript.app_dir", getwd()), "www", "translations.json"),
  simplifyVector = TRUE
)
app_translations$fr <- jsonlite::read_json(
  file.path(getOption("landscript.app_dir", getwd()), "www", "translations-fr.json"),
  simplifyVector = TRUE
)

normalize_ui_language <- function(language) {
  if (identical(language, "en")) "en" else if (identical(language, "fr")) "fr" else "pt-BR"
}

ui_translate <- function(text, language = "pt-BR") {
  language <- normalize_ui_language(language)
  if (language == "pt-BR") return(text)
  dictionary <- unlist(app_translations[[if (language == "fr") "fr" else "text"]], use.names = TRUE)
  fragments <- app_translations$fragments
  fragments <- fragments[order(nchar(fragments), decreasing = TRUE)]
  # Use one pass over the original message so translations cannot translate
  # each other. The same longest-match rule is used by www/i18n.js.
  translate_one <- function(value) {
    if (is.na(value)) return(value)
    core <- trimws(value)
    if (core %in% names(dictionary)) {
      return(substring_replace(value, core, dictionary[[core]]))
    }
    value <- gsub("([0-9]+) de ([0-9]+)",
                  if (language == "fr") "\\1 sur \\2" else "\\1 of \\2", value)
    parts <- character()
    while (nzchar(value)) {
      positions <- vapply(fragments, function(key) {
        regexpr(key, value, fixed = TRUE)[[1]]
      }, integer(1))
      found <- which(positions > 0L)
      if (!length(found)) return(paste0(paste0(parts, collapse = ""), value))
      index <- found[[which.min(positions[found])]]
      start <- positions[[index]]
      key <- fragments[[index]]
      parts <- c(parts, substr(value, 1L, start - 1L), dictionary[[key]])
      value <- substring(value, start + nchar(key))
    }
    paste0(parts, collapse = "")
  }
  vapply(as.character(text), translate_one, character(1), USE.NAMES = FALSE)
}

substring_replace <- function(text, source, replacement) {
  start <- regexpr(source, text, fixed = TRUE)[[1]]
  paste0(substr(text, 1L, start - 1L), replacement,
         substring(text, start + nchar(source)))
}

language_menu <- function(language = "pt-BR") {
  language <- normalize_ui_language(language)
  label <- switch(language, en = "Language", fr = "Language / Langue", "Language / Idioma")
  labels <- c(`pt-BR` = "Português (Brasil)", en = "English", fr = "Français")
  input_ids <- c(`pt-BR` = "language_pt", en = "language_en", fr = "language_fr")
  flags <- c(`pt-BR` = "br", en = "us", fr = "fr")
  shiny::div(
    class = "dropdown language-menu",
    `data-no-i18n` = "true",
    shiny::tags$button(
      id = "language_menu_toggle",
      type = "button",
      class = "btn btn-outline-light btn-sm dropdown-toggle language-menu-toggle",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
      shiny::icon("language"),
      label
    ),
    shiny::tags$ul(
      class = "dropdown-menu dropdown-menu-end",
      `aria-labelledby` = "language_menu_toggle",
      lapply(names(labels), function(value) {
        selected <- identical(language, value)
        shiny::tags$li(
          shiny::actionLink(
            unname(input_ids[[value]]),
            shiny::tagList(
              shiny::span(
                class = "language-option-check",
                if (selected) shiny::icon("check")
              ),
              shiny::img(
                src = paste0("landscript/flags/", flags[[value]], ".svg"),
                class = "language-flag", alt = "", `aria-hidden` = "true",
                width = 24, height = 16
              ),
              unname(labels[[value]])
            ),
            class = paste("dropdown-item language-menu-option", if (selected) "active"),
            lang = value,
            `aria-current` = if (selected) "true" else NULL
          )
        )
      })
    )
  )
}

datatable_language <- function(language = "pt-BR") {
  if (normalize_ui_language(language) == "fr") {
    return(list(
      search = "Rechercher :", lengthMenu = "Afficher _MENU_ lignes",
      info = "_START_–_END_ sur _TOTAL_ lignes", infoEmpty = "0 ligne",
      infoFiltered = "(filtrées parmi _MAX_ lignes au total)",
      emptyTable = "Aucune donnée disponible", zeroRecords = "Aucun résultat correspondant",
      loadingRecords = "Chargement…", processing = "Traitement…",
      paginate = list(first = "Première", last = "Dernière", previous = "Précédente", `next` = "Suivante"),
      aria = list(sortAscending = ": activer pour trier par ordre croissant", sortDescending = ": activer pour trier par ordre décroissant")
    ))
  }
  if (normalize_ui_language(language) == "en") {
    return(list(
      search = "Search:", lengthMenu = "Show _MENU_ rows",
      info = "_START_–_END_ of _TOTAL_ rows", infoEmpty = "0 rows",
      infoFiltered = "(filtered from _MAX_ rows)",
      emptyTable = "No data available", zeroRecords = "No matching records found",
      loadingRecords = "Loading...", processing = "Processing...",
      paginate = list(first = "First", last = "Last", previous = "Previous", `next` = "Next"),
      aria = list(sortAscending = ": activate to sort ascending", sortDescending = ": activate to sort descending")
    ))
  }
  list(
    search = "Pesquisar:", lengthMenu = "Mostrar _MENU_ linhas",
    info = "_START_–_END_ de _TOTAL_ linhas", infoEmpty = "0 linhas",
    infoFiltered = "(filtrado de _MAX_ linhas)",
    emptyTable = "Nenhum dado disponível", zeroRecords = "Nenhum registro encontrado",
    loadingRecords = "Carregando...", processing = "Processando...",
    paginate = list(first = "Primeira", last = "Última", previous = "Anterior", `next` = "Próxima"),
    aria = list(sortAscending = ": ativar para ordenar em ordem crescente", sortDescending = ": ativar para ordenar em ordem decrescente")
  )
}
