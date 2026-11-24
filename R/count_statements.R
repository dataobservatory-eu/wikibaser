#' Count pipe-separated statement values
#'
#' @param x A character vector containing statement values separated
#'   by `" | "`.
#'
#' @returns An integer vector. Missing and empty values return zero.
#' @export
#'
#' @examples
#' count_statements(c("Q1", "Q1 | Q2", NA_character_))
count_statements <- function(x) {
  counts <- lengths(strsplit(x, " | ", fixed = TRUE))

  counts[is.na(x) | !nzchar(trimws(x))] <- 0L

  counts
}
