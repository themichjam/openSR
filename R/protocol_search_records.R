prisma_p_required_fields <- function() {
  c(
    "title", "review_question", "rationale", "objectives", "eligibility_criteria",
    "information_sources", "search_strategy_summary", "outcomes", "analysis_plan"
  )
}

prisma_s_required_search_fields <- function() {
  c(
    "search_id", "source", "platform", "query", "date", "n_results",
    "query_hash", "export_file", "notes"
  )
}

#' Initialize protocol object aligned to PRISMA-P
#' @param title Protocol title.
#' @param review_question Main review question.
#' @return `sr_protocol` object.
#' @export
sr_protocol_init <- function(title, review_question) {
  checkmate::assert_string(title, min.chars = 3)
  checkmate::assert_string(review_question, min.chars = 3)

  data <- tibble::tibble(
    title = title,
    review_question = review_question,
    rationale = NA_character_,
    objectives = NA_character_,
    eligibility_criteria = NA_character_,
    information_sources = NA_character_,
    search_strategy_summary = NA_character_,
    outcomes = NA_character_,
    analysis_plan = NA_character_,
    registration = NA_character_
  )

  new_sr_object(
    data = data,
    metadata = list(standard = "PRISMA-P", status = "draft"),
    provenance = make_provenance("protocol_init"),
    class_name = "sr_protocol"
  )
}

#' Validate PRISMA-P protocol completeness
#' @param protocol `sr_protocol` object.
#' @return Updated `sr_protocol` object.
#' @export
sr_protocol_validate <- function(protocol) {
  checkmate::assert_class(protocol, "sr_protocol")
  validate_sr_object(protocol)

  assert_has_cols(protocol$data, prisma_p_required_fields(), "protocol$data")

  miss <- names(protocol$data)[purrr::map_lgl(protocol$data, ~ all(is.na(.x) | trimws(.x) == ""))]
  hard_required <- c("title", "review_question")
  hard_missing <- intersect(miss, hard_required)

  protocol$validation <- append_validation(
    valid = length(hard_missing) == 0,
    messages = c(
      if (length(hard_missing) > 0) paste("Missing mandatory protocol fields:", paste(hard_missing, collapse = ", ")) else character(),
      if (length(miss) > 0) paste("Incomplete PRISMA-P fields:", paste(miss, collapse = ", ")) else character()
    )
  )
  append_provenance(protocol, "protocol_validate", details = list(missing_fields = miss))
}

#' Export protocol object to YAML
#' @param protocol `sr_protocol` object.
#' @param path Output path.
#' @return Path invisibly.
#' @export
sr_protocol_export <- function(protocol, path) {
  checkmate::assert_class(protocol, "sr_protocol")
  fs::dir_create(dirname(path), recurse = TRUE)
  yaml::write_yaml(as.list(protocol$data[1, ]), path)
  digest_value <- hash_file(path)
  protocol <- append_provenance(protocol, "protocol_export", details = list(path = path, sha256 = digest_value))
  attr(protocol, "export_path") <- path
  protocol
}

#' Initialize PRISMA-S style search log
#' @return `sr_search_log` object.
#' @export
sr_search_log_init <- function() {
  data <- tibble::tibble(
    search_id = character(),
    source = character(),
    platform = character(),
    query = character(),
    date = character(),
    n_results = integer(),
    query_hash = character(),
    export_file = character(),
    notes = character()
  )

  new_sr_object(
    data,
    metadata = list(standard = "PRISMA-S", schema = "search_log_v1"),
    provenance = make_provenance("search_log_init"),
    class_name = "sr_search_log"
  )
}

#' Add search record with provenance hash capture
#' @param search_log `sr_search_log` object.
#' @param source Source name (e.g. PubMed).
#' @param query Query text.
#' @param date Search date.
#' @param n_results Number of records retrieved.
#' @param platform Optional platform label.
#' @param export_file Optional exported result filepath.
#' @param notes Optional notes.
#' @return Updated `sr_search_log` object.
#' @export
sr_search_add <- function(search_log,
                          source,
                          query,
                          date = Sys.Date(),
                          n_results = NA_integer_,
                          platform = NA_character_,
                          export_file = NA_character_,
                          notes = NA_character_) {
  checkmate::assert_class(search_log, "sr_search_log")
  checkmate::assert_string(source, min.chars = 1)
  checkmate::assert_string(query, min.chars = 1)
  if (!is.na(n_results) && n_results < 0) cli::cli_abort("{.arg n_results} cannot be negative.")

  id <- paste0("search_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", nrow(search_log$data) + 1)
  row <- tibble::tibble(
    search_id = id,
    source = source,
    platform = platform,
    query = query,
    date = as.character(date),
    n_results = as.integer(n_results),
    query_hash = digest::digest(paste(source, platform, query, date, sep = "|"), algo = "sha256"),
    export_file = export_file,
    notes = notes
  )

  search_log$data <- dplyr::bind_rows(search_log$data, row)
  search_log <- append_provenance(search_log, "search_add", details = as.list(row))
  search_log
}

#' Validate imported records object
#' @param records `sr_records` object.
#' @return Updated `sr_records` object.
#' @export
sr_records_validate <- function(records) {
  checkmate::assert_class(records, "sr_records")
  validate_sr_object(records)

  assert_has_cols(records$data, c("record_id", "title"), "records$data")

  dup_ids <- records$data$record_id[duplicated(records$data$record_id)]
  empty_titles <- which(is.na(records$data$title) | trimws(records$data$title) == "")

  valid <- length(dup_ids) == 0 && length(empty_titles) == 0
  msgs <- c(
    if (length(dup_ids) > 0) paste("Duplicate record_id values:", paste(unique(dup_ids), collapse = ", ")) else character(),
    if (length(empty_titles) > 0) paste("Empty titles at row(s):", paste(empty_titles, collapse = ", ")) else character()
  )

  records$validation <- append_validation(valid, msgs)
  records
}

normalize_records <- function(data) {
  out <- tibble::as_tibble(data)
  names(out) <- tolower(names(out))
  if (!"record_id" %in% names(out)) out$record_id <- seq_len(nrow(out))
  if (!"source_file" %in% names(out)) out$source_file <- NA_character_
  if (!"source_format" %in% names(out)) out$source_format <- NA_character_
  if (!"title" %in% names(out)) out$title <- NA_character_
  out
}

#' Import RIS records
#' @param path RIS file path.
#' @return `sr_records` object.
#' @export
sr_import_ris <- function(path) {
  checkmate::assert_file_exists(path)
  lines <- readLines(path, warn = FALSE)
  if (!any(stringr::str_detect(lines, "^TY  -"))) {
    cli::cli_abort("Unsupported RIS input: missing required TY tag.")
  }
  title_lines <- lines[stringr::str_detect(lines, "^TI  -")]
  if (length(title_lines) == 0) {
    cli::cli_abort("Unsupported RIS input: no TI title tags found.")
  }
  titles <- stringr::str_trim(stringr::str_replace(title_lines, "^TI  -", ""))
  data <- tibble::tibble(
    record_id = seq_along(titles),
    title = titles,
    source_file = basename(path),
    source_format = "RIS"
  )

  new_sr_object(
    data = data,
    metadata = list(format = "RIS", file = path),
    provenance = make_provenance("import_ris", details = list(path = path, n = nrow(data))),
    class_name = "sr_records"
  )
}

#' Import BibTeX records
#' @param path BibTeX file path.
#' @return `sr_records` object.
#' @export
sr_import_bib <- function(path) {
  checkmate::assert_file_exists(path)
  lines <- readLines(path, warn = FALSE)
  if (!any(stringr::str_detect(lines, "^@"))) {
    cli::cli_abort("Unsupported BibTeX input: no entry header found.")
  }
  title_lines <- lines[stringr::str_detect(lines, "^\\s*title\\s*=")]
  if (length(title_lines) == 0) {
    cli::cli_abort("Unsupported BibTeX input: no title field found.")
  }
  titles <- stringr::str_match(title_lines, "^\\s*title\\s*=\\s*[\\{\"](.*)[\\}\"],?\\s*$")[, 2]
  if (all(is.na(titles) | trimws(titles) == "")) {
    cli::cli_abort("Unsupported BibTeX input: failed to parse title values.")
  }
  data <- tibble::tibble(
    record_id = seq_along(titles),
    title = titles,
    source_file = basename(path),
    source_format = "BIB"
  )

  new_sr_object(
    data = data,
    metadata = list(format = "BIB", file = path),
    provenance = make_provenance("import_bib", details = list(path = path, n = nrow(data))),
    class_name = "sr_records"
  )
}

#' Import CSV records
#' @param path CSV path.
#' @return `sr_records` object.
#' @export
sr_import_csv <- function(path) {
  checkmate::assert_file_exists(path)
  data <- normalize_records(readr::read_csv(path, show_col_types = FALSE))
  data$source_file <- basename(path)
  data$source_format <- "CSV"

  new_sr_object(
    data = data,
    metadata = list(format = "CSV", file = path),
    provenance = make_provenance("import_csv", details = list(path = path, n = nrow(data))),
    class_name = "sr_records"
  )
}

#' Deduplicate records with exact and optional fuzzy matching
#'
#' @param records `sr_records` object.
#' @param key_columns Columns used for exact key generation.
#' @param fuzzy Use simple title normalization for fuzzy grouping.
#' @return Updated `sr_records` object with duplicate flags.
#' @export
sr_deduplicate <- function(records, key_columns = c("title", "doi"), fuzzy = TRUE) {
  checkmate::assert_class(records, "sr_records")
  dat <- records$data
  keys <- intersect(key_columns, names(dat))
  if (length(keys) == 0) cli::cli_abort("No valid deduplication key columns found.")

  exact_key <- apply(dplyr::across(dat, dplyr::all_of(keys)), 1, function(x) {
    paste(tolower(trimws(ifelse(is.na(x), "", x))), collapse = "|")
  })

  dat$dup_group_exact <- exact_key
  dat$is_duplicate_exact <- duplicated(dat$dup_group_exact)

  if (isTRUE(fuzzy) && "title" %in% names(dat)) {
    dat$dup_group_fuzzy <- dat$title |>
      tolower() |>
      stringr::str_replace_all("[^a-z0-9]", "")
    dat$is_duplicate_fuzzy <- duplicated(dat$dup_group_fuzzy)
  } else {
    dat$dup_group_fuzzy <- NA_character_
    dat$is_duplicate_fuzzy <- FALSE
  }

  dat$is_duplicate <- dat$is_duplicate_exact | dat$is_duplicate_fuzzy
  dat$record_uid <- paste0("rec_", sprintf("%06d", seq_len(nrow(dat))))

  records$data <- tibble::as_tibble(dat)
  records <- append_provenance(records, "deduplicate", details = list(keys = keys, fuzzy = fuzzy, removed = sum(dat$is_duplicate)))
  records
}
