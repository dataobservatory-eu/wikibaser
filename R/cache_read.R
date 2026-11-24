#' Read a cached Wikibase entity response
#'
#' @param cache A Wikibase entity cache.
#' @param id A Wikibase entity identifier.
#'
#' @returns A list containing the cached response and its freshness
#'   information, or NULL if no cached response exists.
#' @keywords keyword internal
#' @noRd
cache_read <- function(cache, id) {
  cache <- check_entity_cache(cache)
  file <- file.path(cache$path, paste0(id, ".json"))

  if (!file.exists(file)) {
    return(NULL)
  }

  info <- file.info(file)

  age_hours <- as.numeric(
    difftime(Sys.time(), info$mtime, units = "hours")
  )

  list(
    id = id,
    file = file,
    body = readBin(file, what = "raw", n = info$size),
    retrieved_at = info$mtime,
    age_hours = age_hours,
    fresh = age_hours >= 0 &&
      age_hours < cache$max_age_hours
  )
}
