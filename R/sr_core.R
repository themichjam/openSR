#' opensr object schema version
#' @keywords internal
.opensr_schema_version <- "0.1.0"

#' Construct an OpenSR object
#'
#' @param data Main payload.
#' @param metadata Named list of object metadata.
#' @param provenance A tibble of provenance events.
#' @param validation Named list with `valid` and `messages`.
#' @param version Schema version string.
#' @param class_name Primary S3 class to apply.
#'
#' @return A typed OpenSR S3 object.
#' @export
new_sr_object <- function(data,
                          metadata = list(),
                          provenance = tibble::tibble(),
                          validation = list(valid = TRUE, messages = character()),
                          version = .opensr_schema_version,
                          class_name) {
  checkmate::assert_string(class_name, min.chars = 3)
  checkmate::assert_list(metadata, names = "named")
  checkmate::assert_data_frame(provenance, min.rows = 0)
  checkmate::assert_list(validation, names = "named")
  checkmate::assert_string(version)

  out <- structure(
    list(
      data = data,
      metadata = metadata,
      provenance = provenance,
      validation = validation,
      version = version
    ),
    class = c(class_name, "sr_object", "list")
  )

  validate_sr_object(out)
}

#' Coerce list to OpenSR object
#' @param x List-like object.
#' @param class_name Target class name.
#' @return Typed OpenSR object.
#' @export
as.sr_object <- function(x, class_name) {
  checkmate::assert_list(x)
  checkmate::assert_string(class_name)
  required <- c("data", "metadata", "provenance", "validation", "version")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    cli::cli_abort(c("Invalid object.", x = "Missing required fields: {.val {missing}}"))
  }
  class(x) <- c(class_name, "sr_object", class(x))
  validate_sr_object(x)
}

#' Validate base sr_object invariants
#' @param x An object created by [new_sr_object()].
#' @return `x` invisibly.
#' @export
validate_sr_object <- function(x) {
  if (!inherits(x, "sr_object")) {
    cli::cli_abort("Object is not an {.cls sr_object}.")
  }

  missing <- setdiff(c("data", "metadata", "provenance", "validation", "version"), names(x))
  if (length(missing) > 0) {
    cli::cli_abort(c("Broken sr_object invariant.", x = "Missing fields: {.val {missing}}"))
  }

  checkmate::assert_list(x$metadata, names = "named")
  checkmate::assert_data_frame(x$provenance, min.rows = 0)
  checkmate::assert_list(x$validation, names = "named")
  checkmate::assert_string(x$version)

  invisible(x)
}

make_provenance <- function(event, actor = Sys.info()[["user"]], details = list()) {
  tibble::tibble(
    timestamp = as.character(Sys.time()),
    event = event,
    actor = actor %||% "unknown",
    details = jsonlite::toJSON(details, auto_unbox = TRUE, null = "null")
  )
}

append_provenance <- function(x, event, details = list()) {
  x$provenance <- dplyr::bind_rows(x$provenance, make_provenance(event, details = details))
  x
}

append_validation <- function(valid, messages = character()) {
  list(valid = isTRUE(valid), messages = as.character(messages))
}

#' @export
print.sr_project <- function(x, ...) {
  validate_sr_object(x)
  cat(glue::glue("<sr_project> {x$metadata$name %||% 'unnamed'}\n"))
  cat(glue::glue("Root: {x$metadata$root %||% 'NA'}\n"))
  invisible(x)
}

#' @export
print.sr_protocol <- function(x, ...) {
  validate_sr_object(x)
  cat("<sr_protocol>\n")
  cat(glue::glue("Title: {x$data$title[[1]] %||% 'NA'}\n"))
  invisible(x)
}

#' @export
print.sr_records <- function(x, ...) {
  validate_sr_object(x)
  cat(glue::glue("<sr_records> {nrow(x$data)} rows\n"))
  invisible(x)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

hash_file <- function(path) {
  if (!fs::file_exists(path)) return(NA_character_)
  digest::digest(file = path, algo = "sha256")
}

assert_has_cols <- function(data, cols, object_name = "data") {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Column validation failed.",
      x = "{object_name} is missing required columns: {.val {missing}}"
    ))
  }
}
