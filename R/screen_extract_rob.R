#' Screening template
#' @export
sr_screen_template <- function(records) {
  checkmate::assert_class(records, "sr_records")
  tibble::tibble(
    record_id = records$data$record_id,
    reviewer_1 = NA_character_,
    reviewer_2 = NA_character_,
    conflict = FALSE,
    reason = NA_character_
  )
}

#' Import ASReview screening output
#' @param path CSV path.
#' @param stopping_rule Optional stopping-rule description.
#' @export
sr_screen_import_asreview <- function(path, stopping_rule = NA_character_) {
  data <- readr::read_csv(path, show_col_types = FALSE)
  new_sr_object(
    data = tibble::as_tibble(data),
    metadata = list(source = "ASReview", stopping_rule = stopping_rule),
    provenance = make_provenance("screen_import_asreview"),
    class_name = "sr_screen_log"
  )
}

#' Build dual-screening conflict table
#' @export
sr_screen_dual <- function(screen_tbl) {
  out <- screen_tbl |>
    dplyr::mutate(conflict = .data$reviewer_1 != .data$reviewer_2)
  new_sr_object(out, metadata = list(mode = "dual"), provenance = make_provenance("screen_dual"), class_name = "sr_screen_log")
}

#' Resolve dual-screen conflicts
#' @param screen_log sr_screen_log.
#' @param resolutions data.frame with record_id and final_decision.
#' @export
sr_screen_resolve <- function(screen_log, resolutions) {
  checkmate::assert_class(screen_log, "sr_screen_log")
  resolved <- screen_log$data |>
    dplyr::left_join(tibble::as_tibble(resolutions), by = "record_id") |>
    dplyr::mutate(final_decision = .data$final_decision %||% .data$reviewer_1)
  screen_log$data <- resolved
  screen_log
}

#' Full text exclusion log with controlled vocabulary support
#' @param entries data.frame containing record_id and reason.
#' @param allowed_reasons optional controlled vocabulary vector.
#' @export
sr_fulltext_log <- function(entries, allowed_reasons = NULL) {
  d <- tibble::as_tibble(entries)
  if (!is.null(allowed_reasons)) {
    bad <- setdiff(unique(d$reason), allowed_reasons)
    checkmate::assert_true(length(bad) == 0)
  }
  new_sr_object(d, metadata = list(type = "fulltext"), provenance = make_provenance("fulltext_log"), class_name = "sr_fulltext_log")
}

#' PRISMA flow counts from logs
#' @export
sr_flow_data <- function(records, screen_log = NULL, fulltext_log = NULL) {
  ided <- nrow(records$data)
  dedup <- sum(records$data$is_duplicate %||% FALSE)
  screened <- if (is.null(screen_log)) NA_integer_ else nrow(screen_log$data)
  fulltext_excluded <- if (is.null(fulltext_log)) NA_integer_ else nrow(fulltext_log$data)
  tibble::tibble(
    identified = ided,
    duplicates_removed = dedup,
    screened = screened,
    fulltext_excluded = fulltext_excluded,
    included = screened - fulltext_excluded
  )
}

#' Extraction template
#' @param records sr_records object.
#' @export
sr_extract_template <- function(records) {
  checkmate::assert_class(records, "sr_records")
  new_sr_object(
    data = tibble::tibble(
      record_id = records$data$record_id,
      study_id = NA_character_,
      arm = NA_character_,
      timepoint = NA_character_,
      n = NA_real_,
      mean = NA_real_,
      sd = NA_real_,
      event = NA_real_,
      nonevent = NA_real_
    ),
    metadata = list(stage = "template"),
    provenance = make_provenance("extract_template"),
    class_name = "sr_extraction"
  )
}

#' Validate extraction fields
#' @export
sr_extract_validate <- function(extraction) {
  checkmate::assert_class(extraction, "sr_extraction")
  req <- c("record_id", "study_id")
  miss <- setdiff(req, names(extraction$data))
  extraction$validation <- append_validation(length(miss) == 0, miss)
  extraction
}

#' Autofill extraction from bibliographic metadata
#' @export
sr_extract_autofill <- function(extraction, records) {
  checkmate::assert_class(extraction, "sr_extraction")
  checkmate::assert_class(records, "sr_records")
  if ("title" %in% names(records$data)) {
    extraction$data <- extraction$data |>
      dplyr::left_join(records$data |> dplyr::select(.data$record_id, .data$title), by = "record_id")
  }
  extraction
}

#' Detect likely multi-arm studies
#' @export
sr_detect_multiarm <- function(extraction) {
  extraction$data |>
    dplyr::count(.data$study_id, name = "n_arms") |>
    dplyr::filter(.data$n_arms > 2)
}

#' Detect repeated timepoints
#' @export
sr_detect_timepoints <- function(extraction) {
  extraction$data |>
    dplyr::group_by(.data$study_id) |>
    dplyr::summarise(timepoints = dplyr::n_distinct(.data$timepoint, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(.data$timepoints > 1)
}

#' Detect missing fields for effect size calculations
#' @export
sr_detect_missing_effectsize_fields <- function(extraction) {
  extraction$data |>
    dplyr::mutate(
      missing_binary = is.na(.data$event) | is.na(.data$nonevent),
      missing_cont = is.na(.data$mean) | is.na(.data$sd) | is.na(.data$n)
    ) |>
    dplyr::filter(.data$missing_binary | .data$missing_cont)
}

#' Risk-of-bias template
#' @export
sr_rob_template <- function(records) {
  new_sr_object(
    data = tibble::tibble(
      record_id = records$data$record_id,
      domain = c("randomization", "deviations", "missing_data", "outcome_measurement", "selection_reporting"),
      judgement = NA_character_,
      notes = NA_character_
    ),
    metadata = list(method = "manual_judgement"),
    provenance = make_provenance("rob_template"),
    class_name = "sr_rob"
  )
}

#' Validate risk-of-bias coding
#' @export
sr_rob_validate <- function(rob) {
  allowed <- c("low", "some_concerns", "high", NA)
  ok <- all(rob$data$judgement %in% allowed)
  rob$validation <- append_validation(ok, if (!ok) "Invalid judgement values" else character())
  rob
}

#' Summarize risk-of-bias labels
#' @export
sr_rob_summary <- function(rob) {
  rob$data |>
    dplyr::count(.data$domain, .data$judgement, name = "n")
}
