#' Initialise a Wikibase entity cache
#'
#' Creates or opens a local directory for storing retrieved Wikibase
#' entities as JSON responses. Cached responses remain available after
#' their freshness period expires, allowing offline access.
#'
#' Each cache is associated with a single Wikibase API endpoint and,
#' optionally, an authenticated user.
#'
#' @param path Directory for cached responses. Defaults to a temporary
#'   directory. Windows-style path separators are supported.
#' @param url The MediaWiki API URL. Defaults to the Wikidata API.
#' @param max_age_hours Maximum age of a fresh cached response, in hours.
#'   Defaults to 24 hours. Use `Inf` to disable expiration.
#'   Expired responses remain available for offline access.
#' @param username Optional username identifying the authentication scope.
#'   Defaults to `NULL` for unauthenticated access.
#'
#' @returns An object of class `wikibase_entity_cache`.
#' @export
#'
#' @examples
#' cache <- wb_entity_cache()
#' cache$path
wb_entity_cache <- function(
  path = tempfile("wikibaser-cache-"),
  url = "https://www.wikidata.org/w/api.php",
  max_age_hours = 24,
  username = NULL
) {
  if (!is.character(path) || length(path) != 1L ||
    is.na(path) || !nzchar(path)) {
    abort_argument("path", "{.arg path} must be a directory path.")
  }

  if (!is.character(url) || length(url) != 1L ||
    is.na(url) || !nzchar(url)) {
    abort_argument("url", "{.arg url} must be an API URL.")
  }

  if (!is.numeric(max_age_hours) ||
    length(max_age_hours) != 1L ||
    is.na(max_age_hours) ||
    max_age_hours < 0) {
    abort_argument(
      "max_age_hours",
      "{.arg max_age_hours} must be non-negative."
    )
  }

  if (!is.null(username) &&
    (!is.character(username) || length(username) != 1L ||
      is.na(username) || !nzchar(username))) {
    abort_argument(
      "username",
      "{.arg username} must be a username or NULL."
    )
  }

  # Normalise path separators on Windows and expand the home directory.
  if (.Platform$OS.type == "windows") {
    path <- gsub("\\", "/", path, fixed = TRUE)
  }

  path <- path.expand(path)

  # Create the cache directory if it does not exist.
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, mode = "0700")
  }

  if (!dir.exists(path)) {
    cli::cli_abort("Cannot create cache directory {.file {path}}.")
  }

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  # Identify the repository and authentication scope.
  scope <- list(url = url, username = username)
  manifest <- file.path(path, "cache.rds")

  if (file.exists(manifest)) {
    if (!identical(readRDS(manifest), scope)) {
      cli::cli_abort("Cache belongs to a different repository or user.")
    }
  } else {
    if (length(list.files(path, all.files = TRUE, no.. = TRUE))) {
      cli::cli_abort(
        "The cache directory must be empty or an existing entity cache."
      )
    }

    saveRDS(scope, manifest)
  }

  structure(
    list(
      path = path,
      url = url,
      username = username,
      max_age_hours = max_age_hours
    ),
    class = c("wikibase_entity_cache", "list")
  )
}
