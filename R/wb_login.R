#' Log in to a Wikibase instance
#'
#' Authenticates with a Wikibase instance through its MediaWiki API. The
#' function preserves the HTTP cookie session while requesting the login token,
#' submitting the credentials, and requesting the CSRF token.
#'
#' @param url A character scalar containing the Wikibase MediaWiki API URL.
#' @param username A character scalar containing the Wikibase username.
#' @param password A character scalar containing the Wikibase password.
#'
#' @return A `wikibase_session` object containing:
#'
#' * `request`: the authenticated [httr2::request()] object;
#' * `csrf_token`: the CSRF token for authenticated write operations;
#' * `username`: the authenticated username;
#' * `url`: the MediaWiki API URL;
#' * `cookie_file`: the file used to preserve session cookies.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' session <- wb_login(
#'   url = Sys.getenv("WIKIBASE_API"),
#'   username = Sys.getenv("WIKIBASE_USER"),
#'   password = Sys.getenv("WIKIBASE_PASSWORD")
#' )
#'
#' session$csrf_token
#' }
wb_login <- function(url, username, password) {
  if (
    !is.character(url) ||
      length(url) != 1L ||
      is.na(url) ||
      !nzchar(url)
  ) {
    abort_argument(
      "url",
      "`url` must be a non-empty character scalar."
    )
  }

  if (
    !is.character(username) ||
      length(username) != 1L ||
      is.na(username) ||
      !nzchar(username)
  ) {
    abort_argument(
      "username",
      "`username` must be a non-empty character scalar."
    )
  }

  if (
    !is.character(password) ||
      length(password) != 1L ||
      is.na(password) ||
      !nzchar(password)
  ) {
    abort_argument(
      "password",
      "`password` must be a non-empty character scalar."
    )
  }

  cookie_file <- tempfile("wikibaser-cookies-")

  req <- httr2::request(url) |>
    httr2::req_cookie_preserve(cookie_file)

  # Login token
  login_token <- mw_login_token(req)

  # Login
  mw_login(
    req = req,
    username = username,
    password = password,
    login_token = login_token
  )

  # CSRF token
  csrf_token <- mw_csrf_token(req)

  structure(
    list(
      request = req,
      csrf_token = csrf_token,
      username = username,
      url = url,
      cookie_file = cookie_file
    ),
    class = c("wikibase_session", "list")
  )
}
