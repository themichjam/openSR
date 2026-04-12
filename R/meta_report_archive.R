#' Binary effect size wrapper
#' @export
sr_es_binary <- function(data, ai, bi, ci, di, measure = "RR") {
  es <- metafor::escalc(measure = measure, ai = data[[ai]], bi = data[[bi]], ci = data[[ci]], di = data[[di]], data = data)
  new_sr_object(es, metadata = list(type = "binary", measure = measure), provenance = make_provenance("es_binary"), class_name = "sr_effects")
}

#' Continuous effect size wrapper
#' @export
sr_es_continuous <- function(data, m1i, sd1i, n1i, m2i, sd2i, n2i, measure = "SMD") {
  es <- metafor::escalc(measure = measure, m1i = data[[m1i]], sd1i = data[[sd1i]], n1i = data[[n1i]],
                  m2i = data[[m2i]], sd2i = data[[sd2i]], n2i = data[[n2i]], data = data)
  new_sr_object(es, metadata = list(type = "continuous", measure = measure), provenance = make_provenance("es_continuous"), class_name = "sr_effects")
}

#' Generic effect size wrapper
#' @export
sr_es_generic <- function(data, yi, vi) {
  es <- tibble::tibble(yi = data[[yi]], vi = data[[vi]])
  new_sr_object(es, metadata = list(type = "generic"), provenance = make_provenance("es_generic"), class_name = "sr_effects")
}

#' Random-effects meta-analysis wrapper
#' @export
sr_meta_random <- function(effects, sm = "SMD", method.tau = "REML") {
  dat <- effects$data %||% effects
  fit <- meta::metagen(TE = dat$yi, seTE = sqrt(dat$vi), sm = sm, method.tau = method.tau)
  new_sr_object(fit, metadata = list(model = "random", sm = sm), provenance = make_provenance("meta_random"), class_name = "sr_meta")
}

#' Common-effect meta-analysis wrapper
#' @export
sr_meta_common <- function(effects, sm = "SMD") {
  dat <- effects$data %||% effects
  fit <- meta::metagen(TE = dat$yi, seTE = sqrt(dat$vi), sm = sm, common = TRUE, random = FALSE)
  new_sr_object(fit, metadata = list(model = "common", sm = sm), provenance = make_provenance("meta_common"), class_name = "sr_meta")
}

#' Meta-regression wrapper
#' @export
sr_meta_meta_reg <- function(meta_obj, formula) {
  dat <- meta_obj$data
  fit <- metafor::rma(yi = dat$TE, sei = dat$seTE, mods = formula, method = "REML")
  new_sr_object(fit, metadata = list(model = "meta_reg"), provenance = make_provenance("meta_meta_reg"), class_name = "sr_meta")
}

#' Subgroup meta wrapper
#' @export
sr_meta_subgroup <- function(effects, subgroup, sm = "SMD") {
  dat <- effects$data %||% effects
  fit <- meta::metagen(TE = dat$yi, seTE = sqrt(dat$vi), sm = sm, subgroup = dat[[subgroup]])
  new_sr_object(fit, metadata = list(model = "subgroup", subgroup = subgroup), provenance = make_provenance("meta_subgroup"), class_name = "sr_meta")
}

#' Leave-one-out diagnostics
#' @export
sr_meta_leave1out <- function(effects) {
  dat <- effects$data %||% effects
  out <- purrr::map(seq_len(nrow(dat)), function(i) {
    fit <- sr_meta_random(dat[-i, , drop = FALSE])
    tibble::tibble(left_out = i, pooled = fit$data$TE.random)
  })
  dplyr::bind_rows(out)
}

#' Publication bias helper
#' @export
sr_meta_bias <- function(meta_obj) {
  m <- meta_obj$data %||% meta_obj
  list(k = m$k, pval_egger = NA_real_, note = "Use metafor::regtest for formal test")
}

#' Create PRISMA flow table
#' @export
sr_prisma_flow <- function(flow_data) {
  flow_data |>
    dplyr::rename(records_identified = .data$identified, records_screened = .data$screened)
}

#' PRISMA checklist scaffold
#' @param type checklist type.
#' @export
sr_prisma_checklist <- function(type = c("PRISMA 2020", "PRISMA-P", "PRISMA-S")) {
  type <- match.arg(type)
  tibble::tibble(
    standard = type,
    item = c("Title", "Abstract", "Methods", "Results", "Discussion"),
    status = "todo"
  )
}

#' Methods section stub
#' @export
sr_methods_stub <- function(project_name = "Review") {
  rep <- new_sr_object(
    data = glue::glue("Methods for {project_name}: report protocol, search strategy, screening, extraction, RoB, and synthesis steps."),
    metadata = list(section = "methods"),
    provenance = make_provenance("methods_stub"),
    class_name = "sr_report"
  )
  rep
}

#' Results section stub
#' @export
sr_results_stub <- function() {
  new_sr_object(
    data = "Results: include PRISMA flow, study characteristics, RoB summary, effect estimates, heterogeneity, and sensitivity analyses.",
    metadata = list(section = "results"),
    provenance = make_provenance("results_stub"),
    class_name = "sr_report"
  )
}

#' Build deposit-ready archive bundle
#' @param project_path Project root path.
#' @param output_file Archive file name.
#' @export
sr_archive_bundle <- function(project_path = "review", output_file = NULL) {
  if (is.null(output_file)) {
    output_file <- fs::path(project_path, "archive", paste0("opensr-", format(Sys.Date(), "%Y%m%d"), ".zip"))
  }
  old <- setwd(project_path)
  on.exit(setwd(old), add = TRUE)
  files <- list.files(".", recursive = TRUE)
  utils::zip(zipfile = output_file, files = files)
  new_sr_object(data = tibble::tibble(file = output_file), metadata = list(project_path = project_path),
                provenance = make_provenance("archive_bundle"), class_name = "sr_archive")
}

#' Generate OSF manifest with checksums
#' @export
sr_osf_manifest <- function(project_path = "review") {
  files <- list.files(project_path, recursive = TRUE, full.names = TRUE)
  tibble::tibble(
    file = files,
    size = as.numeric(fs::file_info(files)$size),
    sha256 = purrr::map_chr(files, hash_file)
  )
}

#' Upload files via osfr (optional)
#' @param path path to file or directory.
#' @param project osfr project object or id.
#' @export
sr_osf_push <- function(path, project) {
  checkmate::assert_true(requireNamespace("osfr", quietly = TRUE))
  osfr::osf_upload(osfr::osf_proj(project), path)
}

#' Reproducibility report
#' @export
sr_report_reproducibility <- function(project_path = "review") {
  manifest <- sr_osf_manifest(project_path)
  new_sr_object(
    data = list(
      generated_at = as.character(Sys.time()),
      session = utils::capture.output(sessionInfo()),
      hashes = manifest
    ),
    metadata = list(type = "reproducibility"),
    provenance = make_provenance("repro_report"),
    class_name = "sr_report"
  )
}
