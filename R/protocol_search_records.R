#' Initialize protocol object
#' @param title Protocol title.
#' @param question Review question.
#' @export
sr_protocol_init <- function(title, question) {
  data <- tibble::tibble(
    title = title,
    question = question,
    registration = NA_character_,
    eligibility = NA_character_,
    outcomes = NA_character_
  )
  new_sr_object(
    data = data,
    metadata = list(standard = "PRISMA-P"),
    provenance = make_provenance("protocol_init"),
    class_name = "sr_protocol"
  )
}

#' Validate protocol completeness
#' @param protocol Protocol object.
#' @export
sr_protocol_validate <- function(protocol) {
  checkmate::assert_class(protocol, "sr_protocol")
  required <- c("title", "question")
  missing <- required[!nzchar(protocol$data[1, required, drop = TRUE])]
  protocol$validation <- append_validation(length(missing) == 0, missing)
  protocol
}

#' Export protocol YAML
#' @param protocol Protocol object.
#' @param path Output path.
#' @export
sr_protocol_export <- function(protocol, path) {
  checkmate::assert_class(protocol, "sr_protocol")
  yaml::write_yaml(as.list(protocol$data[1, ]), path)
  invisible(path)
}

#' Initialize search log
#' @export
sr_search_log_init <- function() {
  data <- tibble::tibble(
    source = character(),
    query = character(),
    date = character(),
    n_results = integer(),
    query_hash = character()
  )
  new_sr_object(data, metadata = list(standard = "PRISMA-S"),
                provenance = make_provenance("search_log_init"), class_name = "sr_search_log")
}

#' Add search record with provenance hash
#' @param search_log sr_search_log object.
#' @param source Database source.
#' @param query Query text.
#' @param date Search date.
#' @param n_results Number of hits.
#' @export
sr_search_add <- function(search_log, source, query, date = Sys.Date(), n_results = NA_integer_) {
  checkmate::assert_class(search_log, "sr_search_log")
  row <- tibble::tibble(
    source = source,
    query = query,
    date = as.character(date),
    n_results = as.integer(n_results),
    query_hash = digest::digest(paste(source, query, date, sep = "|"), algo = "sha256")
  )
  search_log$data <- dplyr::bind_rows(search_log$data, row)
  search_log$provenance <- dplyr::bind_rows(search_log$provenance, make_provenance("search_add", details = as.list(row)))
  search_log
}

#' Import RIS file
#' @param path RIS path.
#' @export
sr_import_ris <- function(path) {
  lines <- readLines(path, warn = FALSE)
  titles <- stringr::str_split(lines[stringr::str_detect(lines, "^TI  -")], "TI  - ", simplify = TRUE)[, 2]
  data <- tibble::tibble(record_id = seq_along(titles), title = titles)
  new_sr_object(data, metadata = list(format = "RIS"), provenance = make_provenance("import_ris"), class_name = "sr_records")
}

#' Import BibTeX-like file
#' @param path Bib file path.
#' @export
sr_import_bib <- function(path) {
  lines <- readLines(path, warn = FALSE)
  titles <- gsub('^\	?title\\s*=\\s*[\"{](.*)[\"}],?$', '\\1', lines[stringr::str_detect(lines, "title\\s*=")])
  data <- tibble::tibble(record_id = seq_along(titles), title = titles)
  new_sr_object(data, metadata = list(format = "BIB"), provenance = make_provenance("import_bib"), class_name = "sr_records")
}

#' Import CSV records
#' @param path CSV path.
#' @export
sr_import_csv <- function(path) {
  data <- readr::read_csv(path, show_col_types = FALSE)
  if (!"record_id" %in% names(data)) data$record_id <- seq_len(nrow(data))
  new_sr_object(tibble::as_tibble(data), metadata = list(format = "CSV"), provenance = make_provenance("import_csv"), class_name = "sr_records")
}

#' Validate records object
#' @param records sr_records object.
#' @export
sr_records_validate <- function(records) {
  checkmate::assert_class(records, "sr_records")
  missing_cols <- setdiff(c("record_id", "title"), names(records$data))
  records$validation <- append_validation(length(missing_cols) == 0, missing_cols)
  records
}

#' Deduplicate records with standard flags
#' @param records sr_records object.
#' @param key_columns Key columns for deduplication.
#' @export
sr_deduplicate <- function(records, key_columns = c("title", "doi")) {
  checkmate::assert_class(records, "sr_records")
  keys <- intersect(key_columns, names(records$data))
  checkmate::assert_true(length(keys) > 0)
  d <- records$data |>
    dplyr::mutate(dedupe_key = apply(dplyr::across(dplyr::all_of(keys)), 1, paste, collapse = "|"),
                  dedupe_key = tolower(dedupe_key)) |>
    dplyr::mutate(is_duplicate = duplicated(.data$dedupe_key))
  records$data <- d
  records$provenance <- dplyr::bind_rows(records$provenance, make_provenance("deduplicate", details = list(keys = keys)))
  records
}
