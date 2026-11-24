#' Resolve the source of a Wikibase entity
#'
#' @param session An optional Wikibase session.
#' @param url An optional MediaWiki API URL.
#' @param cache An optional Wikibase entity cache.
#'
#' @returns The resolved MediaWiki API URL.
#' @keywords internal
#' @noRd
resolve_entity_source <- function(session, url, cache) {
  if (!is.null(session)) {
    session <- check_session(session)

    if (!is.null(url) && !identical(url, session$url)) {
      cli::cli_abort("The API URL does not match the session.")
    }

    url <- session$url
  }

  if (!is.null(cache)) {
    cache <- check_entity_cache(cache)

    if (!is.null(url) && !identical(url, cache$url)) {
      cli::cli_abort("The API URL does not match the entity cache.")
    }

    if (!is.null(session) &&
      !identical(session$username, cache$username)) {
      cli::cli_abort(
        "The session user does not match the entity cache."
      )
    }

    url <- cache$url
  }

  if (is.null(url)) {
    url <- "https://www.wikidata.org/w/api.php"
  }

  url
}
