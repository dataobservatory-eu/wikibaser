#' Retrieve a MediaWiki login token
#'
#' Requests a login token from a MediaWiki Action API.
#'
#' @param req An `httr2::request` object pointing to `api.php`.
#'
#' @returns
#' A character string of length one containing the login token.
#'
#' @noRd
#' @importFrom httr2 req_url_query req_perform resp_body_json
mw_login_token <- function(req) {
  resp <-
    req |>
    httr2::req_url_query(
      action = "query",
      meta = "tokens",
      type = "login",
      format = "json"
    ) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(resp)

  token <- body$query$tokens$logintoken

  if (
    !is.character(token) ||
      length(token) != 1L ||
      is.na(token) ||
      !nzchar(token)
  ) {
    abort_api("MediaWiki did not return a valid login token.")
  }

  token
}
