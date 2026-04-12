#' Initialize optional targets pipeline scaffold
#' @param project_path Project path.
#' @export
sr_init_targets <- function(project_path = "review") {
  path <- fs::path(project_path, "_targets.R")
  lines <- c(
    "library(targets)",
    "tar_option_set(packages = c('opensr'))",
    "list(",
    "  tar_target(records, sr_import_csv('search/records.csv'))",
    "  tar_target(flow, sr_flow_data(sr_deduplicate(records)))",
    ")"
  )
  writeLines(lines, path)
  invisible(path)
}

#' Initialize optional renv scaffold
#' @param project_path Project path.
#' @export
sr_init_renv <- function(project_path = "review") {
  path <- fs::path(project_path, "config", "renv-note.txt")
  writeLines("Run renv::init() to activate lockfile-based reproducibility.", path)
  invisible(path)
}
