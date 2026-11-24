test_that("cache_read reads the Q42 fixture", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  cached <- cache_read(cache, "Q42")

  expect_type(cached, "list")
  expect_identical(cached$id, "Q42")
  expect_true(file.exists(cached$file))
  expect_type(cached$body, "raw")
  expect_gt(length(cached$body), 0L)
  expect_s3_class(cached$retrieved_at, "POSIXct")
  expect_type(cached$age_hours, "double")
  expect_true(cached$fresh)

  entity <- parse_entity_response(cached$body, "Q42")

  expect_identical(entity$id, "Q42")
  expect_identical(entity$labels$mul$value, "Douglas Adams")
})


test_that("cache_read preserves the original response bytes", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  file <- file.path(cache$path, "Q42.json")
  body <- readBin(file, what = "raw", n = file.info(file)$size)

  cached <- cache_read(cache, "Q42")

  expect_identical(cached$body, body)
  expect_identical(cached$retrieved_at, file.info(file)$mtime)
})


test_that("cache_read returns NULL for an uncached entity", {
  cache <- wb_entity_cache()

  on.exit(unlink(cache$path, recursive = TRUE))

  expect_null(cache_read(cache, "Q42"))
})


test_that("cache_read identifies fresh and stale responses", {
  cache <- wb_entity_cache(max_age_hours = 24)

  on.exit(unlink(cache$path, recursive = TRUE))

  fixture <- system.file(
    "extdata", "wikidata-cache", "Q42.json",
    package = "wikibaser",
    mustWork = TRUE
  )

  file <- file.path(cache$path, "Q42.json")

  expect_true(file.copy(fixture, file))

  # One hour old: fresh.
  expect_true(Sys.setFileTime(file, Sys.time() - 3600))

  cached <- cache_read(cache, "Q42")

  expect_true(cached$fresh)
  expect_equal(cached$age_hours, 1, tolerance = 0.01)

  # Forty-eight hours old: stale.
  expect_true(Sys.setFileTime(file, Sys.time() - 48 * 3600))

  cached <- cache_read(cache, "Q42")

  expect_false(cached$fresh)
  expect_equal(cached$age_hours, 48, tolerance = 0.01)

  # Stale responses remain readable.
  expect_type(cached$body, "raw")
  expect_identical(
    parse_entity_response(cached$body, "Q42")$id,
    "Q42"
  )
})


test_that("cache_read respects the maximum cache age", {
  cache <- wb_entity_cache(max_age_hours = 0)

  on.exit(unlink(cache$path, recursive = TRUE))

  fixture <- system.file(
    "extdata", "wikidata-cache", "Q42.json",
    package = "wikibaser",
    mustWork = TRUE
  )

  file <- file.path(cache$path, "Q42.json")

  expect_true(file.copy(fixture, file))
  expect_true(Sys.setFileTime(file, Sys.time() - 3600))

  expect_false(cache_read(cache, "Q42")$fresh)

  cache$max_age_hours <- Inf

  expect_true(cache_read(cache, "Q42")$fresh)
})


test_that("cache_read treats future timestamps as stale", {
  cache <- wb_entity_cache(max_age_hours = Inf)

  on.exit(unlink(cache$path, recursive = TRUE))

  fixture <- system.file(
    "extdata", "wikidata-cache", "Q42.json",
    package = "wikibaser",
    mustWork = TRUE
  )

  file <- file.path(cache$path, "Q42.json")

  expect_true(file.copy(fixture, file))
  expect_true(Sys.setFileTime(file, Sys.time() + 3600))

  cached <- cache_read(cache, "Q42")

  expect_lt(cached$age_hours, 0)
  expect_false(cached$fresh)
})


test_that("cache_read rejects invalid caches", {
  expect_error(
    cache_read(list(), "Q42"),
    class = "wikibaser_invalid_cache"
  )
})
