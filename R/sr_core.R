#' Construct an OpenSR object
#' @param data Main data payload.
#' @param metadata Metadata list.
#' @param provenance Provenance tibble.
#' @param validation Validation list.
#' @param version Object schema version.
#' @param class_name S3 class name.
#' @export
new_sr_object <- function(data, metadata = list(), provenance = tibble::tibble(),
                          validation = list(valid = TRUE, messages = character()),
                          version = "0.0.1", class_name) {
  structure(
    list(
      data = data,
      metadata = metadata,
      provenance = provenance,
      validation = validation,
      version = version
    ),
    class = c(class_name, "sr_object", "list")
  )
}

#' Coerce list to OpenSR object
#' @param x List-like object.
#' @param class_name S3 class name.
#' @export
as.sr_object <- function(x, class_name) {
  required <- c("data", "metadata", "provenance", "validation", "version")
  checkmate::assert_list(x)
  checkmate::assert_subset(required, choices = names(x))
  class(x) <- c(class_name, "sr_object", class(x))
  x
}

make_provenance <- function(event, actor = Sys.info()[["user"]], details = list()) {
  tibble::tibble(
    timestamp = as.character(Sys.time()),
    event = event,
    actor = actor,
    details = jsonlite::toJSON(details, auto_unbox = TRUE)
  )
}

append_validation <- function(valid, messages = character()) {
  list(valid = isTRUE(valid), messages = messages)
}

#' @export
print.sr_project <- function(x, ...) {
  cat(glue::glue("<sr_project> {x$metadata$name %||% 'unnamed'}\n"))
  cat(glue::glue("Root: {x$metadata$root %||% 'NA'}\n"))
  invisible(x)
}

#' @export
print.sr_protocol <- function(x, ...) {
  cat("<sr_protocol>\n")
  cat(glue::glue("Title: {x$data$title %||% 'NA'}\n"))
  invisible(x)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

hash_file <- function(path) {
  if (!fs::file_exists(path)) return(NA_character_)
  digest::digest(file = path, algo = "sha256")
}
