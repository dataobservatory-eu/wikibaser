#' Parse a Wikibase entity response
#'
#' @param body A raw vector containing a JSON response.
#' @param id A Wikibase entity identifier.
#'
#' @returns A nested list containing the requested entity.
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @keywords internal
#' @noRd
parse_entity_response <- function(body, id) {
  response <- jsonlite::fromJSON(
    rawToChar(body),
    simplifyVector = FALSE
  )

  if (!is.null(response$error)) {
    cli::cli_abort(
      "Wikibase API error: {response$error$info}",
      class = "wikibaser_api_error"
    )
  }

  entity <- response$entities[[id]]

  if (is.null(entity) || !is.null(entity$missing)) {
    cli::cli_abort(
      "Entity {.val {id}} was not found.",
      class = "wikibaser_entity_not_found"
    )
  }

  entity
}
