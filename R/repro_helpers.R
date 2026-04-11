#' Initialize optional targets pipeline scaffold
#' @param project_path Project path.
#' @return Path to generated `_targets.R`.
#' @export
sr_init_targets <- function(project_path = "review") {
  checkmate::assert_directory_exists(project_path)
  path <- fs::path(project_path, "_targets.R")
  lines <- c(
    "library(targets)",
    "tar_option_set(packages = c('opensr', 'readr', 'dplyr'))",
    "list(",
    "  tar_target(records_raw, sr_import_csv('search/records.csv'))",
    "  tar_target(records, sr_deduplicate(records_raw))",
    "  tar_target(flow, sr_flow_data(records))",
    "  tar_target(prisma_flow, sr_prisma_flow(flow))",
    ")"
  )
  writeLines(lines, path)
  if (fs::dir_exists(fs::path(project_path, "logs"))) {
    sr_log_event(project_path, "init_targets", details = list(path = path, sha256 = hash_file(path)))
  }
  invisible(path)
}

#' Write renv bootstrap note
#' @param project_path Project path.
#' @return Path to generated note.
#' @export
sr_init_renv <- function(project_path = "review") {
  checkmate::assert_directory_exists(project_path)
  path <- fs::path(project_path, "config", "renv-note.txt")
  fs::dir_create(dirname(path), recurse = TRUE)
  writeLines(c(
    "Run renv::init() to initialize project-local dependency management.",
    "Then run renv::snapshot() after updating dependencies."
  ), path)
  if (fs::dir_exists(fs::path(project_path, "logs"))) {
    sr_log_event(project_path, "init_renv", details = list(path = path, sha256 = hash_file(path)))
  }
  invisible(path)
}
