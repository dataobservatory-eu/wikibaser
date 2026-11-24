#' Retrieve multilingual Wikibase entity descriptions
#'
#' Retrieves the labels, descriptions and aliases of a Wikibase
#' item or property and represents them as a tibble in long or
#' wide format.
#'
#' @param id A Wikibase item or property identifier, such as
#'   `"Q42"` or `"P31"`.
#' @param session An optional authenticated Wikibase session
#'   returned by [wb_login()].
#' @param equivalency_property An optional Wikibase property
#'   identifier containing an equivalent entity identifier
#'   from another repository.
#' @param format Character string specifying the output format.
#'   Either `"long"` (default) or `"wide"`.
#' @param languages An optional character vector of Wikibase
#'   language codes, such as `c("en", "hu", "nl")`.
#'   Defaults to `NULL`, returning all available languages.
#' @param cache An optional entity cache created with
#'   [wb_entity_cache()].
#' @param path An optional directory containing entity JSON files.
#'   Passed to [wb_get_entity()] for direct local retrieval without
#'   authentication, cache management or API requests.
#' @param policy Entity retrieval policy passed to [wb_get_entity()].
#'   Use `"cache-only"` for offline retrieval without authentication.
#'
#' @details
#' In long format, each row represents a language in which
#' at least one label, description or alias is available.
#'
#' In wide format, descriptive fields are represented by
#' language-specific columns, such as `label_en`, `label_hu`,
#' `description_en` and `alias_hu`.
#'
#' Language codes are preserved as supplied by the repository,
#' including codes such as `"mul"`, `"en-gb"` and `"zh-hans"`.
#' Requested languages without descriptive metadata are omitted.
#'
#' Multiple aliases within a language are separated by `" | "`.
#' Missing or empty descriptive values are represented by
#' `NA_character_`.
#'
#' If `equivalency_property` is supplied, non-deprecated
#' statements containing valid item or property identifiers
#' are examined. If exactly one distinct equivalent identifier
#' is found, it is returned in the `equivalent_id` column.
#' If multiple distinct identifiers are found, a warning is
#' issued and the equivalent identifier is set to `NA`.
#'
#' Entity retrieval follows the policies of [wb_get_entity()].
#' When `path` is supplied, entities are read directly from
#' local JSON files, bypassing cache management and API retrieval.
#'
#' @returns A tibble containing multilingual entity metadata.
#'   In long format, the columns are `id`, `language`, `label`,
#'   `description` and `alias`.
#'   In wide format, descriptive fields are represented by
#'   language-specific columns.
#'   An additional `equivalent_id` column is included when
#'   `equivalency_property` is supplied.
#'   If no descriptive metadata matches the language selection,
#'   a zero-row tibble is returned.
#'
#' @seealso [wb_get_entity()], [wb_entity_cache()],
#'   [wb_entity_assertions()]
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
#' wb_entity_description("Q42", path = path)
#'
#' wb_entity_description(
#'   "Q42",
#'   path = path,
#'   languages = c("mul", "az", "en", "hu"),
#'   format = "wide"
#' )
wb_entity_description <- function(
  id,
  session = NULL,
  equivalency_property = NULL,
  format = c("long", "wide"),
  languages = NULL,
  cache = NULL,
  policy = c("prefer-cache", "prefer-api", "cache-only", "api-only"),
  path = NULL
) {
  format <- match.arg(format)

  if (is.null(path)) {
    policy <- match.arg(policy)
  }

  if (!is.null(languages) &&
    (!is.character(languages) || anyNA(languages) ||
      any(!nzchar(trimws(languages))))) {
    abort_argument(
      "languages",
      "{.arg languages} must be a character vector of language codes."
    )
  }

  if (!is.null(equivalency_property) &&
    (!is.character(equivalency_property) ||
      length(equivalency_property) != 1L ||
      is.na(equivalency_property) ||
      !grepl("^P[1-9][0-9]*$", equivalency_property))) {
    abort_argument(
      "equivalency_property",
      "{.arg equivalency_property} must be a property identifier."
    )
  }

  entity <- wb_get_entity(
    id = id,
    session = session,
    cache = cache,
    policy = policy,
    path = path
  )

  langs <- as.character(sort(unique(c(
    names(entity$labels),
    names(entity$descriptions),
    names(entity$aliases)
  ))))

  if (!is.null(languages)) {
    langs <- intersect(langs, languages)
  }

  label <- vapply(langs, function(lang) {
    value <- entity$labels[[lang]]$value
    if (is.null(value)) NA_character_ else value
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)

  description <- vapply(langs, function(lang) {
    value <- entity$descriptions[[lang]]$value
    if (is.null(value)) NA_character_ else value
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)

  alias <- vapply(langs, function(lang) {
    values <- vapply(
      entity$aliases[[lang]],
      function(x) x$value,
      FUN.VALUE = character(1)
    )

    values <- values[!is.na(values) & nzchar(trimws(values))]

    if (!length(values)) {
      return(NA_character_)
    }

    paste(values, collapse = " | ")
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)

  label[!is.na(label) & !nzchar(trimws(label))] <- NA_character_
  description[!is.na(description) &
    !nzchar(trimws(description))] <- NA_character_

  result <- tibble::tibble(
    id = rep(id, length(langs)),
    language = langs,
    label = label,
    description = description,
    alias = alias
  )

  result <- result[
    !is.na(result$label) |
      !is.na(result$description) |
      !is.na(result$alias),
  ]

  if (!is.null(equivalency_property)) {
    statements <- entity$claims[[equivalency_property]]

    equivalent_ids <- vapply(statements, function(statement) {
      if (identical(statement$rank, "deprecated")) {
        return(NA_character_)
      }

      value <- statement$mainsnak$datavalue$value

      if (is.list(value)) {
        value <- value$id
      }

      if (!is.character(value) || length(value) != 1L ||
        is.na(value) || !grepl("^[QP][1-9][0-9]*$", value)) {
        return(NA_character_)
      }

      value
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)

    equivalent_ids <- unique(equivalent_ids[!is.na(equivalent_ids)])

    equivalent_id <- NA_character_

    if (length(equivalent_ids) == 1L) {
      equivalent_id <- equivalent_ids
    }

    if (length(equivalent_ids) > 1L) {
      cli::cli_warn(
        "Multiple equivalent identifiers found for {.val {id}}.",
        class = "wikibaser_multiple_equivalent_ids"
      )
    }

    result <- tibble::add_column(
      result,
      equivalent_id = rep(equivalent_id, nrow(result)),
      .after = "id"
    )
  }

  if (format == "wide") {
    result <- tidyr::pivot_wider(
      result,
      names_from = "language",
      values_from = c("label", "description", "alias")
    )
  }

  result
}
