#' Retrieve Wikibase entity assertions
#'
#' Returns the asserted property values of a Wikibase item or
#' property as a one-row tibble.
#'
#' Scalar values and entity identifiers are returned as character
#' strings. Structured values are serialised as compact JSON.
#' Deprecated statements are excluded.
#'
#' @param id A Wikibase item or property identifier.
#' @param session An optional authenticated session returned by
#'   [wb_login()].
#' @param equivalency_property An optional property identifier
#'   containing an equivalent entity identifier in another Wikibase.
#' @param multiple How to represent multiple values of a property.
#'   `"collapse"` separates values with `" | "`.
#'   `"widen"` creates sequentially numbered property columns.
#'
#' @returns A one-row tibble with an `id` column, an optional
#'   `equivalent_id` column and dynamically generated property
#'   columns. All columns are character vectors.
#' @export
#'
#' @examples
#' \donttest{
#' wb_entity_assertions("Q42")
#' wb_entity_assertions("Q42", multiple = "widen")
#' }
wb_entity_assertions <- function(
  id,
  session = NULL,
  equivalency_property = NULL,
  multiple = c("collapse", "widen")
) {
  multiple <- match.arg(multiple)

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

  entity <- wb_get_entity(id = id, session = session)

  # Extract and convert assertion values.
  assertions <- lapply(entity$claims, function(statements) {
    values <- vapply(statements, function(statement) {
      if (identical(statement$rank, "deprecated")) {
        return(NA_character_)
      }

      value <- statement$mainsnak$datavalue$value

      if (is.null(value)) {
        return(NA_character_)
      }

      # Entity identifiers.
      if (is.list(value) && !is.null(value$id)) {
        return(as.character(value$id))
      }

      # Scalar values.
      if (is.atomic(value) && length(value) == 1L) {
        return(as.character(value))
      }

      # Structured values.
      as.character(jsonlite::toJSON(value, auto_unbox = TRUE))
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)

    values <- values[!is.na(values)]

    if (!length(values)) NA_character_ else values
  })

  result <- list(id = id)

  # Extract an equivalent entity identifier.
  if (!is.null(equivalency_property)) {
    equivalent_ids <- assertions[[equivalency_property]]

    equivalent_ids <- unique(equivalent_ids[
      !is.na(equivalent_ids) &
        grepl("^[QP][1-9][0-9]*$", equivalent_ids)
    ])

    result$equivalent_id <- NA_character_

    if (length(equivalent_ids) == 1L) {
      result$equivalent_id <- equivalent_ids
    }

    if (length(equivalent_ids) > 1L) {
      cli::cli_warn(
        "Multiple equivalent identifiers found for {.val {id}}.",
        class = "wikibaser_multiple_equivalent_ids"
      )
    }
  }

  # Construct property columns.
  for (property in names(assertions)) {
    values <- assertions[[property]]

    if (multiple == "collapse") {
      result[[property]] <- if (all(is.na(values))) {
        NA_character_
      } else {
        paste(values, collapse = " | ")
      }
    } else {
      columns <- paste0(property, "_", seq_along(values))
      result[columns] <- as.list(values)
    }
  }

  tibble::as_tibble(result)
}
