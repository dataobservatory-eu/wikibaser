#' Upload a file to a Wikibase instance
#'
#' Upload a local file to the MediaWiki file repository associated with a
#' Wikibase instance. The uploaded file becomes available in the `File:`
#' namespace and can subsequently be referenced from Wikibase entities or
#' other wiki pages.
#'
#' This function is a wrapper around the MediaWiki `action=upload` API and
#' requires an authenticated session created with [wb_login()].
#'
#' @param session An authenticated Wikibase session returned by [wb_login()].
#' @param file Path to the local file to upload.
#' @param filename Optional name under which the file will be stored on the
#'   wiki. Defaults to the basename of `file`.
#' @param text Optional initial wikitext for the file description page
#'   (`File:<filename>`). This may include plain text or templates such as
#'   `{{Information}}`.
#' @param comment Optional upload summary recorded in the MediaWiki upload log.
#' @param ignorewarnings Logical. If `TRUE`, instruct MediaWiki to continue
#'   despite non-fatal warnings, such as an existing filename or duplicate
#'   file. Defaults to `FALSE`.
#'
#' @returns A list describing the uploaded file as returned by the MediaWiki
#'   API, including the stored filename and associated metadata.
#'
#' @seealso [wb_login()]
#'
#' @export
#' @importFrom curl form_file
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom httr2 req_method req_body_multipart resp_body_json
#' @importFrom httr2 req_perform

wb_upload_file <- function(session,
                           file,
                           filename = basename(file),
                           text = NULL,
                           comment = NULL,
                           ignorewarnings = FALSE) {
  session <- check_session(session)
  file <- check_upload_file(file)

  if (!is.character(filename) || length(filename) != 1L || is.na(filename)) {
    cli::cli_abort("{.arg filename} must be a single character string.")
  }

  req <- session$request |>
    httr2::req_method("POST") |>
    httr2::req_body_multipart(
      action = "upload",
      filename = filename,
      file = curl::form_file(file),
      text = text,
      comment = comment,
      token = session$csrf_token,
      ignorewarnings = if (ignorewarnings) "1" else "0",
      format = "json"
    )

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)

  if (!identical(body$upload$result, "Success")) {
    cli::cli_abort(
      c(
        "Upload failed.",
        "x" = "{body$upload$result %||% 'Unknown MediaWiki error'}"
      )
    )
  }

  cli::cli_alert_success("Uploaded {.file {filename}}.")

  body$upload
}
