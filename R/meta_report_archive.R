#' Calculate binary effect sizes via metafor
#' @param data Data frame.
#' @param ai,bi,ci,di Column names for 2x2 counts.
#' @param measure Effect-size measure passed to [metafor::escalc()].
#' @return `sr_effects` object.
#' @export
sr_es_binary <- function(data, ai, bi, ci, di, measure = "RR") {
  checkmate::assert_data_frame(data, min.rows = 1)
  cols <- c(ai, bi, ci, di)
  assert_has_cols(data, cols)

  es <- metafor::escalc(measure = measure, ai = data[[ai]], bi = data[[bi]], ci = data[[ci]], di = data[[di]], data = data)
  new_sr_object(es, metadata = list(type = "binary", measure = measure), provenance = make_provenance("es_binary"), class_name = "sr_effects")
}

#' Calculate continuous effect sizes via metafor
#' @return `sr_effects` object.
#' @export
sr_es_continuous <- function(data, m1i, sd1i, n1i, m2i, sd2i, n2i, measure = "SMD") {
  checkmate::assert_data_frame(data, min.rows = 1)
  cols <- c(m1i, sd1i, n1i, m2i, sd2i, n2i)
  assert_has_cols(data, cols)

  bad_n <- which(data[[n1i]] <= 0 | data[[n2i]] <= 0)
  if (length(bad_n) > 0) cli::cli_abort("Non-positive sample sizes found for continuous outcomes.")

  es <- metafor::escalc(
    measure = measure,
    m1i = data[[m1i]], sd1i = data[[sd1i]], n1i = data[[n1i]],
    m2i = data[[m2i]], sd2i = data[[sd2i]], n2i = data[[n2i]],
    data = data
  )
  new_sr_object(es, metadata = list(type = "continuous", measure = measure), provenance = make_provenance("es_continuous"), class_name = "sr_effects")
}

#' Wrap generic effect sizes
#' @export
sr_es_generic <- function(data, yi, vi) {
  checkmate::assert_data_frame(data, min.rows = 1)
  assert_has_cols(data, c(yi, vi))
  if (any(data[[vi]] <= 0, na.rm = TRUE)) cli::cli_abort("Variance values in {.arg vi} must be positive.")

  es <- tibble::tibble(yi = as.numeric(data[[yi]]), vi = as.numeric(data[[vi]]))
  new_sr_object(es, metadata = list(type = "generic"), provenance = make_provenance("es_generic"), class_name = "sr_effects")
}

as_effect_data <- function(effects) {
  if (inherits(effects, "sr_effects")) return(effects$data)
  checkmate::assert_data_frame(effects)
  effects
}

#' Run random-effects meta-analysis
#' @param effects `sr_effects` object or data frame with `yi`, `vi`.
#' @param sm Summary measure label.
#' @param method.tau Tau estimation method.
#' @return `sr_meta` object.
#' @export
sr_meta_random <- function(effects, sm = "SMD", method.tau = "REML") {
  dat <- as_effect_data(effects)
  assert_has_cols(dat, c("yi", "vi"))
  if (nrow(dat) < 2) cli::cli_abort("Need at least two studies for random-effects meta-analysis.")

  fit <- meta::metagen(TE = dat$yi, seTE = sqrt(dat$vi), sm = sm, method.tau = method.tau)
  new_sr_object(fit, metadata = list(model = "random", sm = sm, method.tau = method.tau), provenance = make_provenance("meta_random"), class_name = "sr_meta")
}

#' Run common-effect meta-analysis
#' @export
sr_meta_common <- function(effects, sm = "SMD") {
  dat <- as_effect_data(effects)
  assert_has_cols(dat, c("yi", "vi"))
  fit <- meta::metagen(TE = dat$yi, seTE = sqrt(dat$vi), sm = sm, common = TRUE, random = FALSE)
  new_sr_object(fit, metadata = list(model = "common", sm = sm), provenance = make_provenance("meta_common"), class_name = "sr_meta")
}

#' Run meta-regression via metafor
#' @param effects `sr_effects` object or data frame with `yi`, `vi` and moderator columns.
#' @param formula Model formula, e.g. `~ moderator`.
#' @return `sr_meta` object.
#' @export
sr_meta_meta_reg <- function(effects, formula) {
  dat <- as_effect_data(effects)
  assert_has_cols(dat, c("yi", "vi"))
  fit <- metafor::rma(yi = dat$yi, vi = dat$vi, mods = formula, method = "REML", data = dat)
  new_sr_object(fit, metadata = list(model = "meta_reg"), provenance = make_provenance("meta_meta_reg"), class_name = "sr_meta")
}

#' Run subgroup meta-analysis
#' @param subgroup Column name for subgroup variable.
#' @return `sr_meta` object.
#' @export
sr_meta_subgroup <- function(effects, subgroup, sm = "SMD") {
  dat <- as_effect_data(effects)
  assert_has_cols(dat, c("yi", "vi", subgroup))
  fit <- meta::metagen(TE = dat$yi, seTE = sqrt(dat$vi), sm = sm, subgroup = dat[[subgroup]])
  new_sr_object(fit, metadata = list(model = "subgroup", subgroup = subgroup), provenance = make_provenance("meta_subgroup"), class_name = "sr_meta")
}

#' Leave-one-out sensitivity analysis
#' @return Tibble with pooled estimates excluding each row.
#' @export
sr_meta_leave1out <- function(effects) {
  dat <- as_effect_data(effects)
  assert_has_cols(dat, c("yi", "vi"))
  if (nrow(dat) < 3) cli::cli_abort("Need at least three studies for leave-one-out analysis.")

  purrr::map_dfr(seq_len(nrow(dat)), function(i) {
    fit <- sr_meta_random(dat[-i, , drop = FALSE])
    tibble::tibble(left_out = i, pooled = fit$data$TE.random)
  })
}

#' Publication-bias checks
#' @param effects `sr_effects` object or data frame.
#' @return Named list.
#' @export
sr_meta_bias <- function(effects) {
  dat <- as_effect_data(effects)
  assert_has_cols(dat, c("yi", "vi"))
  if (nrow(dat) < 3) {
    return(list(k = nrow(dat), pval_egger = NA_real_, note = "At least three studies recommended for bias checks"))
  }
  test <- tryCatch(metafor::regtest(yi = dat$yi, vi = dat$vi), error = function(e) NULL)
  list(
    k = nrow(dat),
    pval_egger = if (is.null(test)) NA_real_ else unname(test$pval),
    note = if (is.null(test)) "regtest failed; inspect data" else "egger_test"
  )
}

#' Build PRISMA 2020 flow table
#' @param flow_report `sr_report` object from [sr_flow_data()].
#' @return `sr_report` object.
#' @export
sr_prisma_flow <- function(flow_report) {
  if (!inherits(flow_report, "sr_report")) {
    flow_report <- new_sr_object(
      data = tibble::as_tibble(flow_report),
      metadata = list(standard = "PRISMA 2020"),
      provenance = make_provenance("flow_wrap"),
      class_name = "sr_report"
    )
  }
  assert_has_cols(flow_report$data, c("identified", "duplicates_removed", "records_after_dedup", "screened", "fulltext_excluded", "included"))

  flow_report$metadata$report_type <- "prisma_flow"
  append_provenance(flow_report, "prisma_flow")
}

#' Create PRISMA checklist scaffold
#' @param type One of `PRISMA 2020`, `PRISMA-P`, `PRISMA-S`.
#' @return `sr_report` object.
#' @export
sr_prisma_checklist <- function(type = c("PRISMA 2020", "PRISMA-P", "PRISMA-S")) {
  type <- match.arg(type)
  items <- switch(type,
    "PRISMA 2020" = c("Title", "Abstract", "Rationale", "Eligibility criteria", "Information sources", "Results of syntheses"),
    "PRISMA-P" = c("Administrative information", "Rationale", "Objectives", "Eligibility criteria", "Data synthesis"),
    "PRISMA-S" = c("Database name", "Platform", "Search dates", "Search strategy", "Limits and filters")
  )

  new_sr_object(
    data = tibble::tibble(standard = type, item = items, status = "todo", location = NA_character_),
    metadata = list(standard = type, report_type = "checklist"),
    provenance = make_provenance("prisma_checklist", details = list(type = type)),
    class_name = "sr_report"
  )
}

#' Methods section stub generated from metadata
#' @param project `sr_project` object.
#' @param protocol Optional `sr_protocol` object.
#' @return `sr_report` object.
#' @export
sr_methods_stub <- function(project, protocol = NULL) {
  checkmate::assert_class(project, "sr_project")
  txt <- c(
    paste0("Review name: ", project$metadata$name %||% "Unnamed review"),
    "Standards intent: PRISMA 2020, PRISMA-P, PRISMA-S (PRISMA-aware; not compliance guarantee).",
    "Describe databases searched, screening process, extraction fields, and synthesis methods."
  )
  if (!is.null(protocol) && inherits(protocol, "sr_protocol")) {
    txt <- c(txt, paste0("Review question: ", protocol$data$review_question[[1]] %||% "(not set)"))
  }

  new_sr_object(
    data = paste(txt, collapse = "\n"),
    metadata = list(section = "methods"),
    provenance = make_provenance("methods_stub"),
    class_name = "sr_report"
  )
}

#' Results section stub generated from flow/meta data
#' @param flow_report Optional `sr_report` from [sr_flow_data()].
#' @param meta_object Optional `sr_meta` object.
#' @return `sr_report` object.
#' @export
sr_results_stub <- function(flow_report = NULL, meta_object = NULL) {
  lines <- c("Report PRISMA flow, study characteristics, risk-of-bias, and synthesis estimates.")
  if (!is.null(flow_report) && inherits(flow_report, "sr_report")) {
    lines <- c(lines, paste0("Included studies (from flow): ", flow_report$data$included[[1]] %||% "NA"))
  }
  if (!is.null(meta_object) && inherits(meta_object, "sr_meta")) {
    lines <- c(lines, paste0("Meta-analysis studies: ", meta_object$data$k %||% "NA"))
  }

  new_sr_object(
    data = paste(lines, collapse = "\n"),
    metadata = list(section = "results"),
    provenance = make_provenance("results_stub"),
    class_name = "sr_report"
  )
}

#' Create deposit-ready archive bundle
#' @param project_path Project root path.
#' @param output_file Optional zip path.
#' @param include_patterns Optional regex include filter.
#' @return `sr_archive` object.
#' @export
sr_archive_bundle <- function(project_path = "review", output_file = NULL, include_patterns = NULL) {
  checkmate::assert_directory_exists(project_path)

  if (is.null(output_file)) {
    output_file <- fs::path(project_path, "archive", paste0("opensr-", format(Sys.Date(), "%Y%m%d"), ".zip"))
  }
  fs::dir_create(dirname(output_file), recurse = TRUE)

  files <- list.files(project_path, recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("/archive/", files)]
  if (!is.null(include_patterns)) {
    keep <- purrr::map_lgl(files, ~ any(stringr::str_detect(.x, include_patterns)))
    files <- files[keep]
  }
  if (length(files) == 0) cli::cli_abort("No files selected for archive bundle.")

  old <- setwd(project_path)
  on.exit(setwd(old), add = TRUE)
  rel <- gsub(paste0("^", normalizePath(project_path), "/?"), "", normalizePath(files))
  utils::zip(zipfile = normalizePath(output_file), files = rel)
  archive_hash <- hash_file(output_file)

  new_sr_object(
    data = tibble::tibble(file = output_file, n_files = length(files), sha256 = archive_hash),
    metadata = list(project_path = project_path),
    provenance = make_provenance("archive_bundle", details = list(output_file = output_file, n_files = length(files), sha256 = archive_hash)),
    class_name = "sr_archive"
  )
}

#' Build OSF-ready file manifest
#' @param project_path Project root path.
#' @return `sr_archive` object.
#' @export
sr_osf_manifest <- function(project_path = "review") {
  checkmate::assert_directory_exists(project_path)
  files <- list.files(project_path, recursive = TRUE, full.names = TRUE)
  files <- files[!dir.exists(files)]

  manifest <- tibble::tibble(
    file = files,
    rel_file = gsub(paste0("^", normalizePath(project_path), "/?"), "", normalizePath(files)),
    size = as.numeric(fs::file_info(files)$size),
    sha256 = purrr::map_chr(files, hash_file)
  )

  manifest_hash <- digest::digest(paste(manifest$sha256, collapse = "|"), algo = "sha256")

  new_sr_object(
    data = manifest,
    metadata = list(type = "osf_manifest", project_path = project_path, manifest_hash = manifest_hash),
    provenance = make_provenance("osf_manifest", details = list(n_files = nrow(manifest), manifest_hash = manifest_hash)),
    class_name = "sr_archive"
  )
}

#' Verify manifest integrity
#' @param manifest `sr_archive` object from [sr_osf_manifest()].
#' @return Logical scalar.
#' @export
sr_archive_verify <- function(manifest) {
  checkmate::assert_class(manifest, "sr_archive")
  assert_has_cols(manifest$data, c("file", "sha256"), "manifest$data")

  current <- purrr::map_chr(manifest$data$file, hash_file)
  all(current == manifest$data$sha256 | (is.na(current) & is.na(manifest$data$sha256)))
}

#' Upload files to OSF via osfr
#' @param path Path to file or folder to upload.
#' @param project OSF project id or osfr object.
#' @param dry_run If `TRUE`, do not upload, return planned action.
#' @return Upload result or planned action list.
#' @export
sr_osf_push <- function(path, project, dry_run = TRUE) {
  checkmate::assert_string(path, min.chars = 1)
  if (!fs::file_exists(path) && !fs::dir_exists(path)) cli::cli_abort("{.arg path} does not exist.")
  if (isTRUE(dry_run)) {
    return(list(action = "upload", path = path, project = project, dry_run = TRUE))
  }

  checkmate::assert_true(requireNamespace("osfr", quietly = TRUE))
  osfr::osf_upload(osfr::osf_proj(project), path)
}

#' Generate reproducibility report
#' @param project_path Project root.
#' @return `sr_report` object.
#' @export
sr_report_reproducibility <- function(project_path = "review") {
  manifest <- sr_osf_manifest(project_path)
  data <- list(
    generated_at = as.character(Sys.time()),
    session_info = utils::capture.output(sessionInfo()),
    package_versions = as.data.frame(utils::installed.packages()[, c("Package", "Version")]),
    hashes = manifest$data
  )

  new_sr_object(
    data = data,
    metadata = list(type = "reproducibility"),
    provenance = make_provenance("repro_report", details = list(project_path = project_path)),
    class_name = "sr_report"
  )
}
