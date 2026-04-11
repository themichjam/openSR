project_layout <- function() {
  c(
    "protocol", "search", "screening", "fulltext", "extraction", "rob",
    "synthesis", "reports", "archive", "data-raw", "data", "logs", "config"
  )
}

#' Create systematic review project skeleton
#'
#' @param path Root path for the review project.
#' @param name Human-readable project name.
#' @param write_readme Write a short project README in the created project folder.
#' @return A `sr_project` object.
#' @export
sr_project_create <- function(path = "review", name = basename(path), write_readme = TRUE) {
  checkmate::assert_string(path, min.chars = 1)
  checkmate::assert_string(name, min.chars = 1)

  fs::dir_create(path, recurse = TRUE)
  purrr::walk(project_layout(), ~ fs::dir_create(fs::path(path, .x), recurse = TRUE))

  cfg <- list(
    name = name,
    created_at = as.character(Sys.time()),
    standards = c("PRISMA 2020", "PRISMA-P", "PRISMA-S"),
    foss_only = TRUE,
    osf_optional = TRUE,
    prisma_aware_only = TRUE
  )
  sr_config_write(cfg, fs::path(path, "config", "opensr.yml"))
  sr_log_event(path, "project_created", details = list(name = name, layout = project_layout()))

  if (isTRUE(write_readme)) {
    writeLines(c(
      paste0("# ", name),
      "",
      "This folder was created by opensr.",
      "PRISMA-aware workflow support is provided, but compliance is not guaranteed."
    ), fs::path(path, "README.md"))
  }

  new_sr_object(
    data = list(paths = project_layout()),
    metadata = list(name = name, root = path),
    provenance = make_provenance("project_create", details = list(path = path)),
    class_name = "sr_project"
  )
}

#' Load review project configuration
#' @param path Review project root path.
#' @return A `sr_project` object.
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

#' Read opensr config file
#' @param path Path to YAML config.
#' @return Named list.
#' @export
sr_config_read <- function(path) {
  checkmate::assert_file_exists(path)
  cfg <- yaml::read_yaml(path)
  checkmate::assert_list(cfg, names = "named")
  cfg
}

#' Write opensr config file
#' @param config Named list.
#' @param path Output YAML path.
#' @return Path invisibly.
#' @export
sr_config_write <- function(config, path) {
  checkmate::assert_list(config, names = "named")
  fs::dir_create(dirname(path), recurse = TRUE)
  yaml::write_yaml(config, path)
  invisible(path)
}

#' Append a project event log entry
#'
#' Logs are written to both CSV (`logs/events.csv`) and ndjson
#' (`logs/events.ndjson`) for machine-readable provenance.
#'
#' @param project_path Review root path.
#' @param event Event name.
#' @param details Named list with event details.
#' @return Path to CSV log invisibly.
#' @export
sr_log_event <- function(project_path, event, details = list()) {
  checkmate::assert_directory_exists(project_path)
  checkmate::assert_string(event, min.chars = 1)
  checkmate::assert_list(details, names = "named", null.ok = TRUE)

  log_dir <- fs::path(project_path, "logs")
  fs::dir_create(log_dir)

  csv_path <- fs::path(log_dir, "events.csv")
  ndjson_path <- fs::path(log_dir, "events.ndjson")

  ts <- as.character(Sys.time())
  details_json <- as.character(jsonlite::toJSON(details, auto_unbox = TRUE, null = "null"))
  row <- tibble::tibble(
    timestamp = ts,
    event = event,
    event_id = digest::digest(paste(ts, event, details_json, sep = "|"), algo = "sha256"),
    details = details_json
  )

  if (fs::file_exists(csv_path)) {
    prior <- readr::read_csv(csv_path, show_col_types = FALSE)
    out <- dplyr::bind_rows(prior, row)
  } else {
    out <- row
  }
  readr::write_csv(out, csv_path)

  cat(as.character(jsonlite::toJSON(as.list(row[1, ]), auto_unbox = TRUE)), "\n", file = ndjson_path, append = TRUE)
  invisible(csv_path)
}
