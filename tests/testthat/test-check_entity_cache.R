test_that("check_entity_cache accepts the Wikidata cache fixture", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  expect_identical(check_entity_cache(cache), cache)
  expect_false(withVisible(check_entity_cache(cache))$visible)
})


test_that("check_entity_cache rejects invalid cache objects", {
  invalid_caches <- list(
    NULL,
    list(),
    character(),
    "wikidata-cache",
    42,
    TRUE
  )

  for (cache in invalid_caches) {
    expect_error(
      check_entity_cache(cache),
      class = "wikibaser_invalid_cache"
    )
  }
})


test_that("check_entity_cache rejects a cache with a missing directory", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  cache$path <- tempfile("nonexistent-cache-")

  expect_error(
    check_entity_cache(cache),
    regexp = "does not exist",
    class = "wikibaser_invalid_cache"
  )
})


test_that("check_entity_cache rejects an ordinary list", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  expect_error(
    check_entity_cache(unclass(cache)),
    class = "wikibaser_invalid_cache"
  )
})
