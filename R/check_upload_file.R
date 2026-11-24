#' Validate a local file for upload
#'
#' Checks that a file on the local filesystem exists and can be used as the
#' source of a MediaWiki upload. The validated path is returned in normalized
#' form for use in subsequent upload requests.
#'
#' @param file Path to a file on the local filesystem to be uploaded.
#'
#' @returns Invisibly returns the normalized path to `file`.
#'
#' @noRd
check_upload_file <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    cli::cli_abort(
      "{.arg file} must be a single file path.",
      class = "wikibaser_invalid_file"
    )
  }

  if (!nzchar(file)) {
    cli::cli_abort(
      "{.arg file} must not be an empty string.",
      class = "wikibaser_invalid_file"
    )
  }

  if (!file.exists(file)) {
    cli::cli_abort(
      c(
        "Cannot find local file.",
        "x" = "{.file {file}} does not exist."
      ),
      class = "wikibaser_file_not_found"
    )
  }

  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}
