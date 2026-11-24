#' Retrieve a Wikibase entity
#'
#' Retrieves an item or property from a Wikibase repository,
#' a managed entity cache or a directory of local JSON files.
#'
#' @param id A Wikibase item or property identifier, such as
#'   `"Q42"` or `"P31"`.
#' @param session An optional authenticated Wikibase session
#'   returned by [wb_login()].
#' @param url An optional Wikibase API endpoint. If omitted,
#'   the endpoint is determined from the session or cache.
#'   Otherwise, the Wikidata API is used.
#' @param as Character string specifying the return format.
#'   Either `"list"` (default) or `"json"`.
#' @param cache An optional entity cache created with
#'   [wb_entity_cache()].
#' @param policy Character string specifying the cache policy.
#'   One of `"prefer-cache"` (default), `"prefer-api"`,
#'   `"cache-only"` or `"api-only"`.
#' @param path An optional directory containing entity JSON files
#'   named by their identifiers, such as `Q42.json`.
#'   If supplied, the entity is read directly from this directory.
#'   No authentication, cache manifest or API connection is required.
#'
#' @details
#' When `path` is supplied, the function reads the original JSON
#' response from the specified directory. The session, URL,
#' cache and retrieval policy are ignored. Local files are
#' neither refreshed nor modified.
#'
#' Otherwise, the function retrieves entities through the
#' Wikibase `wbgetentities` API action or a managed entity cache.
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
#'     Retrieves the entity exclusively from the managed cache,
#'     regardless of its age.
#'   }
#'   \item{`"api-only"`}{
#'     Retrieves the entity directly from the API without reading
#'     or updating cached responses.
#'   }
#' }
#'
#' A warning is issued whenever a stale managed cache response
#' is used. Successfully retrieved and validated API responses
#' are stored in the supplied cache unless the `"api-only"`
#' policy is selected.
#'
#' When a session, URL and cache are supplied together, their
#' repository endpoints must agree. An authenticated cache can
#' be refreshed only with a matching authenticated session.
#'
#' Cache fallback applies to API connection failures, not to
#' API errors or missing entities.
#'
#' @returns
#' If `as = "list"`, a nested list containing the parsed entity,
#' including its identifier, descriptive metadata and statements.
#'
#' If `as = "json"`, the path to a temporary JSON file containing
#' the complete original API response. This file is independent
#' of the original local or cached file.
#'
#' @seealso [wb_entity_cache()], [wb_login()]
#'
#' @export
#'
#' @examples
#' path <- system.file(
#'   "extdata", "wikidata-cache",
#'   package = "wikibaser",
#'   mustWork = TRUE
#' )
#'
#' # Read an entity directly from a local JSON file.
#' entity <- wb_get_entity("Q42", path = path)
#'
#' entity$id
#' entity$labels$mul$value
#'
#' # Retrieve a copy of the original JSON response.
#' json_file <- wb_get_entity(
#'   "Q42",
#'   path = path,
#'   as = "json"
#' )
#'
#' file.exists(json_file)
#' unlink(json_file)
#'
#' # Use a managed entity cache.
#' cache <- wb_entity_cache(
#'   path = path,
#'   max_age_hours = Inf
#' )
#'
#' wb_get_entity(
#'   "Q42",
#'   cache = cache,
#'   policy = "cache-only"
#' )
#'
#' \donttest{
#' wb_get_entity("Q42")
#' wb_get_entity("P31")
#' }
wb_get_entity <- function(
  id,
  session = NULL,
  url = NULL,
  as = c("list", "json"),
  cache = NULL,
  policy = c("prefer-cache", "prefer-api", "cache-only", "api-only"),
  path = NULL
) {
  as <- match.arg(as)

  if (!is.character(id) || length(id) != 1L ||
    is.na(id) || !grepl("^[QP][1-9][0-9]*$", id)) {
    abort_argument(
      "id",
      "{.arg id} must be an item or property identifier."
    )
  }

  # Read an existing local JSON file.
  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1L ||
      is.na(path) || !nzchar(path) || !dir.exists(path)) {
      abort_argument(
        "path",
        "{.arg path} must be an existing directory."
      )
    }

    file <- file.path(path, paste0(id, ".json"))

    if (!file.exists(file)) {
      cli::cli_abort(
        "Entity file {.file {file}} does not exist.",
        class = "wikibaser_entity_file_not_found"
      )
    }

    body <- readBin(
      file,
      what = "raw",
      n = file.info(file)$size
    )

    entity <- parse_entity_response(body, id)

    if (as == "json") {
      json_file <- tempfile(
        pattern = paste0(id, "-"),
        fileext = ".json"
      )

      writeBin(body, json_file)

      return(json_file)
    }

    return(entity)
  }

  # Managed cache and API retrieval.
  policy <- match.arg(policy)

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

  entity <- parse_entity_response(body, id)

  if (!use_cache && !is.null(cache) && policy != "api-only") {
    file <- file.path(cache$path, paste0(id, ".json"))
    writeBin(body, file)
  }

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
