#' Check that an object is a Wikibase session
#'
#' @param session An object expected to inherit from
#'   `"wikibase_session"`.
#'
#' @returns Invisibly returns `session`.
#' @noRd
check_session <- function(session) {
  
  if (!inherits(session, "wikibase_session")) {
    cli::cli_abort(
      c(
        "{.arg session} is not a valid Wikibase session.",
        "i" = "Create a session with {.fn wb_login}."
      ),
      class = "wikibaser_invalid_session"
    )
  }
  
  invisible(session)
}


