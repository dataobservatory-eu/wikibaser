test_that("wb_entity_assertions returns collapsed assertions", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  entity <- wb_get_entity(
    "Q42",
    cache = cache,
    policy = "cache-only"
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL) entity
  )

  result <- wb_entity_assertions("Q42")

  result$P735

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_identical(result$id, "Q42")
  expect_identical(result$P31, "Q5")

  expect_setequal(
    names(result),
    c("id", names(entity$claims))
  )

  expect_true(all(vapply(result, is.character, logical(1))))
})

test_that("wb_entity_assertions widens assertion values", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  entity <- wb_get_entity(
    "Q42",
    cache = cache,
    policy = "cache-only"
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL) entity
  )

  result <- wb_entity_assertions("Q42", multiple = "widen")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_identical(result$id, "Q42")
  expect_identical(result$P31_1, "Q5")

  expect_true(all(
    grepl("^P[0-9]+_[0-9]+$", names(result)[-1])
  ))

  expect_true(all(vapply(result, is.character, logical(1))))
})


test_that("collapsed and widened assertions are consistent", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  entity <- wb_get_entity(
    "Q42",
    cache = cache,
    policy = "cache-only"
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL) entity
  )

  collapsed <- wb_entity_assertions("Q42", multiple = "collapse")
  widened <- wb_entity_assertions("Q42", multiple = "widen")

  for (property in names(entity$claims)) {
    columns <- startsWith(names(widened), paste0(property, "_"))
    values <- unlist(widened[columns], use.names = FALSE)

    expected <- if (all(is.na(values))) {
      NA_character_
    } else {
      paste(values, collapse = " | ")
    }

    expect_identical(collapsed[[property]], expected)
  }
})


test_that("structured assertion values retain their fields", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  entity <- wb_get_entity(
    "Q42",
    cache = cache,
    policy = "cache-only"
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL) entity
  )

  result <- wb_entity_assertions("Q42", multiple = "widen")

  expect_true(jsonlite::validate(result$P569_1))

  value <- jsonlite::fromJSON(result$P569_1)

  expect_true("time" %in% names(value))
  expect_true("precision" %in% names(value))
  expect_true("calendarmodel" %in% names(value))
})
