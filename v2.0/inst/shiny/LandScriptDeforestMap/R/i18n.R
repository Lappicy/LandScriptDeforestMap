# Session language is passed explicitly to modules; never store it in options().
app_translations <- jsonlite::read_json(
  file.path(getOption("landscript.app_dir", getwd()), "www", "translations.json"),
  simplifyVector = TRUE
)

normalize_ui_language <- function(language) {
  if (identical(language, "en")) "en" else "pt-BR"
}

ui_translate <- function(text, language = "pt-BR") {
  if (normalize_ui_language(language) == "pt-BR") return(text)
  dictionary <- unlist(app_translations$text, use.names = TRUE)
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
    value <- gsub("([0-9]+) de ([0-9]+)", "\\1 of \\2", value)
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

language_buttons <- function(language = "pt-BR") {
  shiny::div(
    class = "plot-language-switch",
    role = "group",
    `aria-label` = "Language / Idioma",
    lapply(c("pt-BR", "en"), function(value) {
      selected <- identical(language, value)
      shiny::actionButton(
        if (value == "en") "language_en" else "language_pt",
        if (value == "en") "🇺🇸 English" else "🇧🇷 Português (Brasil)",
        class = paste("btn-sm language-button",
                      if (selected) "btn-primary active" else "btn-outline-secondary"),
        `aria-pressed` = if (selected) "true" else "false",
        title = if (value == "en") "Switch the entire interface to English" else {
          "Traduzir toda a interface para português do Brasil"
        }
      )
    })
  )
}

datatable_language <- function(language = "pt-BR") {
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
