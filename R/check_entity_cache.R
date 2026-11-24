#' Validate a Wikibase entity cache
#'
#' Checks whether an object is a Wikibase entity cache and whether
#' its cache directory exists.
#'
#' @param cache An object to validate.
#'
#' @returns The validated cache object, invisibly.
#' @keywords internal
#' @noRd
check_entity_cache <- function(cache) {
  if (!inherits(cache, "wikibase_entity_cache")) {
    cli::cli_abort(
      c(
        "{.arg cache} is not a valid Wikibase entity cache.",
        "i" = "Create an entity cache with {.fn wb_entity_cache}."
      ),
      class = "wikibaser_invalid_cache"
    )
  }

  if (!dir.exists(cache$path)) {
    cli::cli_abort(
      "Cache directory {.file {cache$path}} does not exist.",
      class = "wikibaser_invalid_cache"
    )
  }

  invisible(cache)
}
