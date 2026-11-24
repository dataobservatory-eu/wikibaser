test_that("wb_get_entity retrieves a cached entity", {
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

  expect_type(entity, "list")
  expect_identical(entity$id, "Q42")
  expect_identical(entity$type, "item")
  expect_identical(entity$labels$mul$value, "Douglas Adams")
})


test_that("wb_get_entity prefers a fresh cached response", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  entity <- wb_get_entity("Q42", cache = cache)

  cached_entity <- wb_get_entity(
    "Q42",
    cache = cache,
    policy = "cache-only"
  )

  expect_identical(entity, cached_entity)
})
