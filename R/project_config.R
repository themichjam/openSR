#' Create systematic review project skeleton
#' @param path Root project path.
#' @param name Review name.
#' @export
sr_project_create <- function(path = "review", name = basename(path)) {
  dirs <- c(
    "protocol", "search", "screening", "fulltext", "extraction", "rob",
    "synthesis", "reports", "archive", "data-raw", "data", "logs", "config"
  )
  fs::dir_create(path)
  purrr::map(dirs, ~ fs::dir_create(fs::path(path, .x)))

  cfg <- list(
    name = name,
    created_at = as.character(Sys.time()),
    standards = c("PRISMA 2020", "PRISMA-P", "PRISMA-S"),
    foss_only = TRUE,
    osf_optional = TRUE
  )
  sr_config_write(cfg, fs::path(path, "config", "opensr.yml"))
  sr_log_event(path, "project_created", details = list(name = name))

  new_sr_object(
    data = list(paths = dirs),
    metadata = list(name = name, root = path),
    provenance = make_provenance("project_create", details = list(path = path)),
    class_name = "sr_project"
  )
}

#' Load a review project
#' @param path Project path.
#' @export
sr_project_load <- function(path = "review") {
  cfg_path <- fs::path(path, "config", "opensr.yml")
  cfg <- sr_config_read(cfg_path)
  new_sr_object(
    data = cfg,
    metadata = list(name = cfg$name %||% basename(path), root = path),
    provenance = make_provenance("project_load", details = list(path = path)),
    class_name = "sr_project"
  )
}

#' Read project config
#' @param path Config path.
#' @export
sr_config_read <- function(path) {
  checkmate::assert_file_exists(path)
  yaml::read_yaml(path)
}

#' Write project config
#' @param config Named list.
#' @param path Output path.
#' @export
sr_config_write <- function(config, path) {
  checkmate::assert_list(config, names = "named")
  fs::dir_create(dirname(path))
  yaml::write_yaml(config, path)
  invisible(path)
}

#' Append event to project log
#' @param project_path Project root path.
#' @param event Event name.
#' @param details Event details.
#' @export
sr_log_event <- function(project_path, event, details = list()) {
  log_path <- fs::path(project_path, "logs", "events.csv")
  row <- tibble::tibble(
    timestamp = as.character(Sys.time()),
    event = event,
    details = jsonlite::toJSON(details, auto_unbox = TRUE)
  )
  if (fs::file_exists(log_path)) {
    prior <- readr::read_csv(log_path, show_col_types = FALSE)
    out <- dplyr::bind_rows(prior, row)
  } else {
    out <- row
  }
  readr::write_csv(out, log_path)
  invisible(log_path)
}
