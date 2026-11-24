test_that("wb_entity_description returns multilingual metadata", {
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

  result <- wb_entity_description("Q42")

  expect_s3_class(result, "tbl_df")

  expect_named(
    result,
    c("id", "language", "label", "description", "alias")
  )

  expect_true(nrow(result) > 0L)
  expect_true(all(result$id == "Q42"))
  expect_identical(anyDuplicated(result$language), 0L)

  expect_identical(
    result$label[result$language == "mul"],
    "Douglas Adams"
  )

  expect_identical(
    result$label[result$language == "az"],
    "Duqlas Adams"
  )
})

test_that("wb_entity_description preserves descriptive languages", {
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

  result <- wb_entity_description("Q42")

  languages <- unique(c(
    names(entity$labels),
    names(entity$descriptions),
    names(entity$aliases)
  ))

  expect_true(all(result$language %in% languages))

  expect_true(all(
    !is.na(result$label) |
      !is.na(result$description) |
      !is.na(result$alias)
  ))

  expect_type(result$id, "character")
  expect_type(result$language, "character")
  expect_type(result$label, "character")
  expect_type(result$description, "character")
  expect_type(result$alias, "character")
})


test_that("wb_entity_description removes completely empty languages", {
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

  entity$labels <- list(
    en = list(language = "en", value = "Douglas Adams"),
    hu = list(language = "hu", value = ""),
    nl = list(language = "nl", value = " ")
  )

  entity$descriptions <- list(
    de = list(language = "de", value = "Schriftsteller")
  )

  entity$aliases <- list(
    fr = list(
      list(language = "fr", value = "Douglas Noël Adams"),
      list(language = "fr", value = "DNA")
    )
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL) entity
  )

  result <- wb_entity_description("Q42")

  expect_setequal(result$language, c("en", "de", "fr"))

  expect_identical(
    result$alias[result$language == "fr"],
    "Douglas Noël Adams | DNA"
  )

  expect_true(is.na(result$label[result$language == "fr"]))
  expect_true(is.na(result$label[result$language == "de"]))
  expect_true(is.na(result$description[result$language == "en"]))
})

test_that("wb_entity_description handles empty metadata", {
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

  entity$labels <- list()
  entity$descriptions <- list()
  entity$aliases <- list()

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL) entity
  )

  result <- wb_entity_description("Q42")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)

  expect_named(
    result,
    c("id", "language", "label", "description", "alias")
  )
})

test_that("wb_entity_description extracts an equivalent identifier", {
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

  entity$claims$P999999999 <- list(
    list(
      rank = "normal",
      mainsnak = list(
        snaktype = "value",
        datavalue = list(
          value = "Q123",
          type = "string"
        )
      )
    )
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL) entity
  )

  result <- wb_entity_description(
    "Q42",
    equivalency_property = "P999999999"
  )

  expect_named(
    result,
    c(
      "id", "equivalent_id", "language",
      "label", "description", "alias"
    )
  )

  expect_true(all(result$equivalent_id == "Q123"))
  expect_true(all(result$id == "Q42"))
})


test_that("wb_entity_description handles missing equivalency", {
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

  entity$claims <- list()

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL) entity
  )

  result <- wb_entity_description(
    "Q42",
    equivalency_property = "P999999999"
  )

  expect_true("equivalent_id" %in% names(result))
  expect_type(result$equivalent_id, "character")
  expect_true(all(is.na(result$equivalent_id)))
})

test_that("wb_entity_description rejects invalid equivalency properties", {
  invalid_properties <- list(
    "",
    NA_character_,
    "Q42",
    "P0",
    "P01",
    "p31",
    31,
    c("P31", "P279")
  )

  for (property in invalid_properties) {
    expect_error(
      wb_entity_description(
        "Q42",
        equivalency_property = property
      ),
      class = "wikibaser_argument_error"
    )
  }
})
