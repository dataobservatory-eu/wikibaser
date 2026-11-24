#' Retrieve multilingual Wikibase entity descriptions
#'
#' Retrieves the labels, descriptions and aliases of a Wikibase item
#' or property. Only languages with at least one non-empty descriptive
#' value are returned.
#'
#' Optionally retrieves an equivalent entity identifier from a
#' designated property linking the entity to another Wikibase.
#'
#' @param id A Wikibase item or property identifier, such as `"Q42"`
#'   or `"P31"`.
#' @param session An optional authenticated session returned by
#'   [wb_login()].
#' @param equivalency_property An optional property identifier
#'   containing an equivalent item or property identifier in another
#'   Wikibase repository.
#'
#' @returns A tibble with columns `id`, `language`, `label`,
#'   `description` and `alias`. If `equivalency_property` is supplied,
#'   an `equivalent_id` column is included after `id`.
#'   Missing values are represented by `NA_character_`.
#'   Multiple aliases are separated by `" | "`.
#' @export
#'
#' @examples
#' \donttest{
#' wb_entity_description("Q42")
#' wb_entity_description("P31")
#' }
wb_entity_description <- function(
  id,
  session = NULL,
  equivalency_property = NULL
) {
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
    session = session
  )

  languages <- sort(unique(c(
    names(entity$labels),
    names(entity$descriptions),
    names(entity$aliases)
  )))

  languages <- as.character(languages)

  label <- vapply(
    languages,
    function(lang) {
      value <- entity$labels[[lang]]$value

      if (is.null(value)) NA_character_ else value
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  description <- vapply(
    languages,
    function(lang) {
      value <- entity$descriptions[[lang]]$value

      if (is.null(value)) NA_character_ else value
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  alias <- vapply(
    languages,
    function(lang) {
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
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  # Represent empty descriptive values as missing.
  label[!is.na(label) & !nzchar(trimws(label))] <- NA_character_

  description[
    !is.na(description) & !nzchar(trimws(description))
  ] <- NA_character_

  result <- tibble::tibble(
    id = rep(id, length(languages)),
    language = languages,
    label = label,
    description = description,
    alias = alias
  )

  # Remove languages without descriptive metadata.
  result <- result[
    !is.na(result$label) |
      !is.na(result$description) |
      !is.na(result$alias), ,
    drop = FALSE
  ]

  # Retrieve an equivalent identifier if requested.
  if (!is.null(equivalency_property)) {
    statements <- entity$claims[[equivalency_property]]

    equivalent_ids <- vapply(
      statements,
      function(statement) {
        if (identical(statement$rank, "deprecated")) {
          return(NA_character_)
        }

        value <- statement$mainsnak$datavalue$value

        if (is.list(value)) {
          value <- value$id
        }

        if (!is.character(value) ||
          length(value) != 1L ||
          is.na(value) ||
          !grepl("^[QP][1-9][0-9]*$", value)) {
          return(NA_character_)
        }

        value
      },
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )

    equivalent_ids <- unique(
      equivalent_ids[!is.na(equivalent_ids)]
    )

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

  result
}
