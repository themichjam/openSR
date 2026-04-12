.allowed_screen_decisions <- c("include", "exclude", "maybe", "unsure")

#' Create dual-screening template
#' @param records `sr_records` object.
#' @param reviewers Character vector of reviewer IDs (length >= 2).
#' @param calibration_round Logical flag for calibration phase.
#' @return `sr_screen_log` object.
#' @export
sr_screen_template <- function(records, reviewers = c("rev_a", "rev_b"), calibration_round = FALSE) {
  checkmate::assert_class(records, "sr_records")
  checkmate::assert_character(reviewers, min.len = 2, unique = TRUE)
  n_rows <- nrow(records$data)

  dat <- tibble::tibble(
    record_id = records$data$record_id,
    reviewer_1 = rep(reviewers[[1]], n_rows),
    decision_1 = NA_character_,
    reviewer_2 = rep(reviewers[[2]], n_rows),
    decision_2 = NA_character_,
    conflict = rep(NA, n_rows),
    queue = NA_character_,
    final_decision = NA_character_,
    reason = NA_character_,
    calibration_round = rep(calibration_round, n_rows)
  )

  new_sr_object(
    data = dat,
    metadata = list(mode = "dual", reviewers = reviewers),
    provenance = make_provenance("screen_template"),
    class_name = "sr_screen_log"
  )
}

#' Import ASReview output
#' @param path CSV export path from ASReview.
#' @param stopping_rule Optional stopping rule string.
#' @return `sr_screen_log` object.
#' @export
sr_screen_import_asreview <- function(path, stopping_rule = NA_character_) {
  checkmate::assert_file_exists(path)
  raw <- readr::read_csv(path, show_col_types = FALSE)

  dat <- tibble::as_tibble(raw)
  if (!"record_id" %in% names(dat)) dat$record_id <- seq_len(nrow(dat))
  if (!"included" %in% names(dat)) {
    cli::cli_warn("ASReview import missing {.field included}; defaulting final decisions to {.val maybe}.")
    dat$included <- NA
  }

  dat <- dat |>
    dplyr::mutate(
      final_decision = dplyr::case_when(
        .data$included %in% c(TRUE, 1, "1", "yes") ~ "include",
        .data$included %in% c(FALSE, 0, "0", "no") ~ "exclude",
        TRUE ~ "maybe"
      )
    )

  new_sr_object(
    data = dat,
    metadata = list(source = "ASReview", stopping_rule = stopping_rule),
    provenance = make_provenance("screen_import_asreview", details = list(path = path, stopping_rule = stopping_rule)),
    class_name = "sr_screen_log"
  )
}

#' Compute conflict table for dual screening
#' @param screen_log `sr_screen_log` or data frame with dual decisions.
#' @return Updated `sr_screen_log` object.
#' @export
sr_screen_dual <- function(screen_log) {
  dat <- if (inherits(screen_log, "sr_screen_log")) screen_log$data else tibble::as_tibble(screen_log)
  n_in <- nrow(dat)
  assert_has_cols(dat, c("record_id", "decision_1", "decision_2"), "screen_log$data")

  bad <- setdiff(unique(stats::na.omit(c(dat$decision_1, dat$decision_2))), .allowed_screen_decisions)
  if (length(bad) > 0) {
    cli::cli_abort(c("Invalid screening decision values.", x = "Unsupported decision(s): {.val {bad}}"))
  }

  dat <- dat |>
    dplyr::mutate(
      conflict = .data$decision_1 != .data$decision_2,
      queue = dplyr::case_when(
        is.na(.data$decision_1) | is.na(.data$decision_2) ~ "incomplete",
        .data$conflict ~ "disagreement",
        TRUE ~ "resolved"
      )
    )
  if (nrow(dat) != n_in) cli::cli_abort("Screening dual operation changed row count unexpectedly.")

  out <- if (inherits(screen_log, "sr_screen_log")) screen_log else new_sr_object(dat, class_name = "sr_screen_log")
  out$data <- dat
  out <- append_provenance(out, "screen_dual", details = list(conflicts = sum(dat$conflict, na.rm = TRUE)))
  class(out) <- unique(c("sr_screen_log", "sr_object", class(out)))
  out
}

#' Resolve screening conflicts
#' @param screen_log `sr_screen_log`.
#' @param resolutions Data frame with `record_id`, `final_decision`, and optional `resolver`.
#' @return Updated `sr_screen_log` object.
#' @export
sr_screen_resolve <- function(screen_log, resolutions) {
  checkmate::assert_class(screen_log, "sr_screen_log")
  res <- tibble::as_tibble(resolutions)
  n_in <- nrow(screen_log$data)
  assert_has_cols(res, c("record_id", "final_decision"), "resolutions")

  bad <- setdiff(unique(stats::na.omit(res$final_decision)), .allowed_screen_decisions)
  if (length(bad) > 0) cli::cli_abort("Invalid final_decision values in resolutions.")

  dat <- screen_log$data |>
    dplyr::left_join(res, by = "record_id", suffix = c("", "_new")) |>
    dplyr::mutate(
      final_decision = dplyr::coalesce(.data$final_decision_new, .data$final_decision),
      queue = dplyr::case_when(
        .data$conflict & is.na(.data$final_decision) ~ "disagreement",
        TRUE ~ "resolved"
      )
    ) |>
    dplyr::select(-dplyr::any_of("final_decision_new"))
  if (nrow(dat) != n_in) cli::cli_abort("Screen resolution changed row count unexpectedly.")

  screen_log$data <- dat
  screen_log <- append_provenance(screen_log, "screen_resolve", details = list(resolved_n = nrow(res)))
  class(screen_log) <- unique(c("sr_screen_log", "sr_object", class(screen_log)))
  screen_log
}

#' Create full-text exclusion log
#' @param entries Data frame with at least `record_id` and `reason`.
#' @param allowed_reasons Controlled vocabulary.
#' @return `sr_fulltext_log` object.
#' @export
sr_fulltext_log <- function(entries,
                            allowed_reasons = c(
                              "wrong population", "wrong intervention", "wrong comparator",
                              "wrong outcomes", "wrong study design", "not primary study", "full text unavailable"
                            )) {
  d <- tibble::as_tibble(entries)
  assert_has_cols(d, c("record_id", "reason"), "entries")

  bad <- setdiff(unique(stats::na.omit(d$reason)), allowed_reasons)
  if (length(d$reason) > 0) {
    for (r in stats::na.omit(d$reason)) checkmate::assert_choice(r, allowed_reasons)
  }
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "Invalid full-text exclusion reasons.",
      x = "Unknown reason(s): {.val {bad}}",
      i = "Use {.arg allowed_reasons} to extend vocabulary explicitly."
    ))
  }

  new_sr_object(
    d,
    metadata = list(type = "fulltext", vocabulary = allowed_reasons),
    provenance = make_provenance("fulltext_log", details = list(n = nrow(d))),
    class_name = "sr_fulltext_log"
  )
}

#' Compute PRISMA flow data from logs
#' @param records `sr_records` object.
#' @param screen_log Optional `sr_screen_log` object.
#' @param fulltext_log Optional `sr_fulltext_log` object.
#' @return `sr_report` object with flow table.
#' @export
sr_flow_data <- function(records, screen_log = NULL, fulltext_log = NULL) {
  checkmate::assert_class(records, "sr_records")

  dat <- records$data
  identified <- nrow(dat)
  duplicates_removed <- sum(dat$is_duplicate %||% FALSE, na.rm = TRUE)
  records_after_dedup <- identified - duplicates_removed

  screened <- if (is.null(screen_log)) records_after_dedup else nrow(screen_log$data)
  fulltext_assessed <- if (is.null(screen_log)) NA_integer_ else sum(screen_log$data$final_decision %in% c("include", "exclude", "maybe"), na.rm = TRUE)
  fulltext_excluded <- if (is.null(fulltext_log)) 0L else nrow(fulltext_log$data)
  included <- if (is.null(screen_log)) NA_integer_ else sum(screen_log$data$final_decision == "include", na.rm = TRUE) - fulltext_excluded

  out <- tibble::tibble(
    identified,
    duplicates_removed,
    records_after_dedup,
    screened,
    fulltext_assessed,
    fulltext_excluded,
    included
  )

  new_sr_object(
    data = out,
    metadata = list(standard = "PRISMA 2020"),
    provenance = make_provenance("flow_data"),
    class_name = "sr_report"
  )
}

#' Create extraction template
#' @param records `sr_records` object.
#' @return `sr_extraction` object.
#' @export
sr_extract_template <- function(records) {
  checkmate::assert_class(records, "sr_records")
  dat <- tibble::tibble(
    record_id = records$data$record_id,
    study_id = NA_character_,
    outcome_id = NA_character_,
    outcome_type = NA_character_, # binary | continuous | generic
    arm = NA_character_,
    comparator = NA_character_,
    timepoint = NA_character_,
    design_cluster = FALSE,
    design_crossover = FALSE,
    n = NA_real_,
    mean = NA_real_,
    sd = NA_real_,
    event = NA_real_,
    nonevent = NA_real_,
    yi = NA_real_,
    vi = NA_real_
  )

  new_sr_object(
    data = dat,
    metadata = list(stage = "template"),
    provenance = make_provenance("extract_template"),
    class_name = "sr_extraction"
  )
}

#' Validate extraction object
#' @param extraction `sr_extraction` object.
#' @return Updated `sr_extraction` object.
#' @export
sr_extract_validate <- function(extraction) {
  checkmate::assert_class(extraction, "sr_extraction")
  assert_has_cols(extraction$data, c("record_id", "study_id", "outcome_type"), "extraction$data")

  dat <- extraction$data
  impossible_n <- which(!is.na(dat$n) & dat$n <= 0)
  impossible_sd <- which(!is.na(dat$sd) & dat$sd < 0)
  invalid_type <- setdiff(unique(stats::na.omit(dat$outcome_type)), c("binary", "continuous", "generic"))

  msgs <- c(
    if (length(impossible_n) > 0) paste("Non-positive n rows:", paste(impossible_n, collapse = ", ")) else character(),
    if (length(impossible_sd) > 0) paste("Negative SD rows:", paste(impossible_sd, collapse = ", ")) else character(),
    if (length(invalid_type) > 0) paste("Invalid outcome_type:", paste(invalid_type, collapse = ", ")) else character()
  )

  extraction$validation <- append_validation(length(msgs) == 0, msgs)
  extraction
}

#' Autofill extraction fields from bibliographic records
#' @param extraction `sr_extraction` object.
#' @param records `sr_records` object.
#' @return Updated `sr_extraction` object.
#' @export
sr_extract_autofill <- function(extraction, records) {
  checkmate::assert_class(extraction, "sr_extraction")
  checkmate::assert_class(records, "sr_records")

  cols <- intersect(c("record_id", "title", "year", "doi"), names(records$data))
  extraction$data <- extraction$data |>
    dplyr::left_join(records$data |> dplyr::select(dplyr::all_of(cols)), by = "record_id")

  append_provenance(extraction, "extract_autofill", details = list(cols = cols))
}

#' Detect likely multi-arm studies
#' @param extraction `sr_extraction` object.
#' @return Tibble with studies with more than 2 arms.
#' @export
sr_detect_multiarm <- function(extraction) {
  checkmate::assert_class(extraction, "sr_extraction")
  extraction$data |>
    dplyr::group_by(.data$study_id) |>
    dplyr::summarise(n_arms = dplyr::n_distinct(.data$arm, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(!is.na(.data$study_id), .data$n_arms > 2)
}

#' Detect repeated timepoints
#' @param extraction `sr_extraction` object.
#' @return Tibble with studies containing repeated timepoints.
#' @export
sr_detect_timepoints <- function(extraction) {
  checkmate::assert_class(extraction, "sr_extraction")
  extraction$data |>
    dplyr::group_by(.data$study_id) |>
    dplyr::summarise(timepoints = dplyr::n_distinct(.data$timepoint, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(!is.na(.data$study_id), .data$timepoints > 1)
}

#' Detect missing effect-size fields by outcome type
#' @param extraction `sr_extraction` object.
#' @return Tibble of problematic rows.
#' @export
sr_detect_missing_effectsize_fields <- function(extraction) {
  checkmate::assert_class(extraction, "sr_extraction")
  extraction$data |>
    dplyr::mutate(
      missing_binary = .data$outcome_type == "binary" & (is.na(.data$event) | is.na(.data$nonevent)),
      missing_continuous = .data$outcome_type == "continuous" & (is.na(.data$mean) | is.na(.data$sd) | is.na(.data$n)),
      missing_generic = .data$outcome_type == "generic" & (is.na(.data$yi) | is.na(.data$vi))
    ) |>
    dplyr::filter(.data$missing_binary | .data$missing_continuous | .data$missing_generic)
}

#' Create risk-of-bias template (manual judgments only)
#' @param records `sr_records` object.
#' @return `sr_rob` object.
#' @export
sr_rob_template <- function(records) {
  checkmate::assert_class(records, "sr_records")

  domains <- c("randomization", "deviations", "missing_data", "outcome_measurement", "selection_reporting")
  dat <- tibble::tibble(
    record_id = rep(records$data$record_id, each = length(domains)),
    domain = rep(domains, times = nrow(records$data)),
    judgement = NA_character_,
    notes = NA_character_
  )

  new_sr_object(
    data = dat,
    metadata = list(method = "manual_judgement", automated_judgement = FALSE),
    provenance = make_provenance("rob_template"),
    class_name = "sr_rob"
  )
}

#' Validate risk-of-bias object
#' @param rob `sr_rob` object.
#' @return Updated `sr_rob` object.
#' @export
sr_rob_validate <- function(rob) {
  checkmate::assert_class(rob, "sr_rob")
  assert_has_cols(rob$data, c("record_id", "domain", "judgement"), "rob$data")
  allowed <- c("low", "some_concerns", "high", NA_character_)

  bad <- setdiff(unique(stats::na.omit(rob$data$judgement)), allowed)
  rob$validation <- append_validation(length(bad) == 0, if (length(bad) > 0) paste("Invalid judgement:", paste(bad, collapse = ", ")))
  rob
}

#' Summarize risk-of-bias coding
#' @param rob `sr_rob` object.
#' @return Tibble summary.
#' @export
sr_rob_summary <- function(rob) {
  checkmate::assert_class(rob, "sr_rob")
  rob$data |>
    dplyr::count(.data$domain, .data$judgement, name = "n")
}
