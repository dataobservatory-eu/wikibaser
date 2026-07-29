#' @noRd
#' @importFrom cli cli_abort
#' @keywords internal
abort_argument <- function(arg, message) {
  cli::cli_abort(
    message,
    class = c("wikibaser_argument_error", "wikibaser_error"),
    arg = arg
  )
}

#' @noRd
#' @importFrom cli cli_abort
#' @keywords internal
abort_login <- function(result = NULL, reason = NULL) {
  message <- "Wikibase login failed."

  if (!is.null(result) && nzchar(result)) {
    message <- paste0(message, " (", result, ")")
  }

  if (!is.null(reason) && nzchar(reason)) {
    message <- paste0(message, ": ", reason)
  }

  cli::cli_abort(
    message,
    class = c(
      "wikibaser_login_error",
      "wikibaser_error"
    )
  )
}

#' @noRd
#' @importFrom cli cli_abort
#' @keywords internal
abort_http <- function(message) {
  cli::cli_abort(
    message,
    class = c(
      "wikibaser_http_error",
      "wikibaser_error"
    )
  )
}

#' @noRd
#' @importFrom cli cli_abort
#' @keywords internal
abort_api <- function(message) {
  cli::cli_abort(
    message,
    class = c(
      "wikibaser_api_error",
      "wikibaser_error"
    )
  )
}

#' @noRd
#' @importFrom cli cli_abort
#' @keywords internal
abort_invalid_api_url <- function() {
  cli::cli_abort(
    "The API URL must end with {.file api.php}.",
    class = c(
      "wikibaser_api_url_error",
      "wikibaser_error"
    )
  )
}
