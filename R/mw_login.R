#' Authenticate with a MediaWiki Action API
#'
#' @param req An `httr2::request`.
#' @param username Bot username.
#' @param password Bot password.
#' @param login_token Login token returned by `mw_login_token()`.
#'
#' @returns
#' The authenticated request object (invisibly).
#'
#' @noRd
mw_login <- function(
  req,
  username,
  password,
  login_token
) {
  resp <- req |>
    httr2::req_method("POST") |>
    httr2::req_body_form(
      action = "login",
      lgname = username,
      lgpassword = password,
      lgtoken = login_token,
      format = "json"
    ) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(resp)

  login_result <- body$login$result

  if (!identical(login_result, "Success")) {
    abort_login(
      result = login_result,
      reason = body$login$reason
    )
  }

  invisible(req)
}
