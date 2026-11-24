#' Retrieve a Wikibase entity
#'
#' Retrieves an item or property from a Wikibase repository or a
#' local entity cache. Supports public and authenticated repositories,
#' configurable cache policies and retrieval of the original JSON
#' response.
#'
#' @param id A character string containing a Wikibase item or property
#'   identifier, such as `"Q42"` or `"P31"`.
#' @param session An optional authenticated Wikibase session returned
#'   by [wb_login()].
#' @param url An optional Wikibase API endpoint. If omitted, the
#'   endpoint is determined from the session or cache. If neither
#'   is supplied, the Wikidata API is used.
#' @param as Character string specifying the return format.
#'   Either `"list"` (default) or `"json"`.
#' @param cache An optional entity cache created with
#'   [wb_entity_cache()].
#' @param policy Character string specifying the cache policy.
#'   One of `"prefer-cache"` (default), `"prefer-api"`,
#'   `"cache-only"` or `"api-only"`. See Details.
#'
#' @details
#' The function retrieves a single entity using the Wikibase
#' `wbgetentities` API action. Both items and properties are supported.
#'
#' The available cache policies are:
#' \describe{
#'   \item{`"prefer-cache"`}{
#'     Uses a fresh cached response when available. Otherwise,
#'     retrieves the entity from the API. Falls back to a cached
#'     response if the API connection fails.
#'   }
#'   \item{`"prefer-api"`}{
#'     Retrieves the entity from the API first. Falls back to a
#'     cached response if the API connection fails.
#'   }
#'   \item{`"cache-only"`}{
#'     Retrieves the entity exclusively from the local cache,
#'     regardless of its age. An error is raised if the entity
#'     is not available in the cache.
#'   }
#'   \item{`"api-only"`}{
#'     Retrieves the entity directly from the API without reading
#'     or updating cached responses.
#'   }
#' }
#'
#' A warning is issued whenever a stale cached response is used.
#' Successfully retrieved and validated API responses are stored
#' in the cache unless the `"api-only"` policy is selected.
#'
#' When a session, URL and cache are supplied together, their
#' repository endpoints must agree. An authenticated cache can
#' be refreshed only with a matching authenticated session.
#' Previously cached entities can be retrieved offline without
#' authentication.
#'
#' Cache fallback applies to API connection failures, not to
#' API errors or missing entities.
#'
#' @returns
#' If `as = "list"`, a nested list containing the parsed entity,
#' including its identifier, descriptive metadata and statements.
#'
#' If `as = "json"`, a character string containing the path to a
#' temporary JSON file with the complete original API response.
#' This file is independent of the cache and may be removed by
#' the caller when no longer needed.
#'
#' @seealso [wb_entity_cache()], [wb_login()]
#'
#' @export
#'
#' @examples
#' # Open the bundled Wikidata example cache.
#' cache <- wb_entity_cache(
#'   path = system.file(
#'     "extdata", "wikidata-cache",
#'     package = "wikibaser",
#'     mustWork = TRUE
#'   ),
#'   max_age_hours = Inf
#' )
#'
#' # Retrieve an entity without an internet connection.
#' entity <- wb_get_entity(
#'   id = "Q42",
#'   cache = cache,
#'   policy = "cache-only"
#' )
#'
#' entity$id
#' entity$labels$mul$value
#'
#' # Retrieve the original JSON response.
#' json_file <- wb_get_entity(
#'   id = "Q42",
#'   as = "json",
#'   cache = cache,
#'   policy = "cache-only"
#' )
#'
#' file.exists(json_file)
#' unlink(json_file)
#'
#' \donttest{
#' # Retrieve an item directly from Wikidata.
#' wb_get_entity("Q42")
#'
#' # Retrieve a property directly from Wikidata.
#' wb_get_entity("P31")
#' }
wb_get_entity <- function(
  id,
  session = NULL,
  url = NULL,
  as = c("list", "json"),
  cache = NULL,
  policy = c("prefer-cache", "prefer-api", "cache-only", "api-only")
) {
  as <- match.arg(as)
  policy <- match.arg(policy)

  if (!is.character(id) || length(id) != 1L ||
    is.na(id) || !grepl("^[QP][1-9][0-9]*$", id)) {
    abort_argument(
      "id",
      "{.arg id} must be an item or property identifier."
    )
  }

  if (is.null(cache) && policy == "cache-only") {
    cli::cli_abort(
      "The {.val cache-only} policy requires an entity cache."
    )
  }

  url <- resolve_entity_source(session, url, cache)

  cached <- NULL

  if (!is.null(cache) && policy != "api-only") {
    cached <- cache_read(cache, id)
  }

  if (policy == "cache-only" && is.null(cached)) {
    cli::cli_abort(
      "Entity {.val {id}} is not available in the cache.",
      class = "wikibaser_cache_miss"
    )
  }

  use_cache <- policy == "cache-only" ||
    (policy == "prefer-cache" &&
      !is.null(cached) && isTRUE(cached$fresh))

  # Retrieve the API response if necessary.
  if (!use_cache) {
    if (!is.null(cache) &&
      !is.null(cache$username) &&
      is.null(session) &&
      policy != "api-only") {
      cli::cli_abort(
        "Refreshing an authenticated entity cache requires a session."
      )
    }

    if (is.null(session)) {
      req <- httr2::request(url)
    } else {
      req <- session$request
    }

    req <- req |>
      httr2::req_method("GET") |>
      httr2::req_url_query(
        action = "wbgetentities",
        ids = id,
        format = "json"
      )

    resp <- tryCatch(
      httr2::req_perform(req),
      httr2_failure = function(e) {
        if (is.null(cached)) {
          stop(e)
        }

        cli::cli_warn(
          "API connection failed. Using cached entity {.val {id}}.",
          class = "wikibaser_cache_fallback"
        )

        NULL
      }
    )

    use_cache <- is.null(resp)
  }

  # Obtain the raw response.
  if (use_cache) {
    body <- cached$body

    if (!isTRUE(cached$fresh)) {
      cli::cli_warn(
        "Using a stale cached entity {.val {id}}.",
        class = "wikibaser_stale_cache"
      )
    }
  } else {
    body <- httr2::resp_body_raw(resp)
  }

  # Validate and parse the response.
  entity <- parse_entity_response(body, id)

  # Store newly retrieved and validated responses.
  if (!use_cache && !is.null(cache) && policy != "api-only") {
    file <- file.path(cache$path, paste0(id, ".json"))
    writeBin(body, file)
  }

  # Return the complete JSON response in a temporary file.
  if (as == "json") {
    file <- tempfile(
      pattern = paste0(id, "-"),
      fileext = ".json"
    )

    writeBin(body, file)

    return(file)
  }

  entity
}
