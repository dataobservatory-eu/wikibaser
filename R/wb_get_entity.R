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
