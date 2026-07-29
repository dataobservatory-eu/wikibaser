#' Retrieve a MediaWiki CSRF token
#'
#' @param req An authenticated `httr2::request`.
#'
#' @returns
#' A character string of length one.
#'
#' @noRd
#' @importFrom httr2 req_url_query req_perform resp_body_json
mw_csrf_token <- function(req) {
  resp <-
    req |>
    httr2::req_url_query(
      action = "query",
      meta = "tokens",
      format = "json"
    ) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(resp)

  token <- body$query$tokens$csrftoken

  if (
    !is.character(token) ||
      length(token) != 1L ||
      is.na(token) ||
      !nzchar(token) ||
      identical(token, "+\\")
  ) {
    abort_api("MediaWiki did not return a valid CSRF token.")
  }

  token
}
