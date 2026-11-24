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


test_that("entities can be read directly from local JSON files", {
  path <- tempfile(tmpdir = tempdir())
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  fixture <- system.file(
    "extdata", "wikidata-cache", "Q42.json",
    package = "wikibaser",
    mustWork = TRUE
  )

  expect_true(file.copy(fixture, path))
  expect_false(file.exists(file.path(path, "cache.rds")))

  entity <- wb_get_entity("Q42", path = path)

  expect_type(entity, "list")
  expect_identical(entity$id, "Q42")
  expect_identical(entity$labels$mul$value, "Douglas Adams")

  json_file <- wb_get_entity("Q42", path = path, as = "json")

  expect_true(file.exists(json_file))
  expect_identical(
    readBin(json_file, "raw", n = file.info(json_file)$size),
    readBin(fixture, "raw", n = file.info(fixture)$size)
  )

  expect_error(
    wb_get_entity("Q999999", path = path),
    class = "wikibaser_entity_file_not_found"
  )

  unlink(json_file)
})


test_that("wb_get_entity rejects invalid identifiers", {
  invalid_ids <- list(
    "", NA_character_, NULL, 42, "Q0", "P0",
    "Q01", "q42", "invalid", c("Q42", "Q43")
  )
  
  for (id in invalid_ids) {
    expect_error(
      wb_get_entity(id),
      class = "wikibaser_argument_error"
    )
  }
})


test_that("wb_get_entity rejects invalid local paths", {
  invalid_paths <- list(
    "", NA_character_, 42,
    tempfile(), c(tempdir(), tempdir())
  )
  
  for (path in invalid_paths) {
    expect_error(
      wb_get_entity("Q42", path = path),
      class = "wikibaser_argument_error"
    )
  }
})


test_that("wb_get_entity rejects invalid retrieval options", {
  expect_error(wb_get_entity("Q42", as = "invalid"))
  expect_error(wb_get_entity("Q42", policy = "invalid"))
  
  expect_error(
    wb_get_entity("Q42", policy = "cache-only"),
    "requires an entity cache"
  )
})

test_that("wb_get_entity reports a cache miss", {
  cache <- wb_entity_cache(path = tempfile())
  on.exit(unlink(cache$path, recursive = TRUE), add = TRUE)
  
  expect_error(
    wb_get_entity("Q42", cache = cache, policy = "cache-only"),
    class = "wikibaser_cache_miss"
  )
})


test_that("wb_get_entity warns about stale cached responses", {
  cache <- wb_entity_cache(
    path = tempfile(),
    max_age_hours = 0
  )
  on.exit(unlink(cache$path, recursive = TRUE), add = TRUE)
  
  fixture <- system.file(
    "extdata", "wikidata-cache", "Q42.json",
    package = "wikibaser",
    mustWork = TRUE
  )
  
  expect_true(file.copy(fixture, cache$path))
  
  expect_warning(
    entity <- wb_get_entity(
      "Q42",
      cache = cache,
      policy = "cache-only"
    ),
    class = "wikibaser_stale_cache"
  )
  
  expect_identical(entity$id, "Q42")
})


test_that("authenticated cache refresh requires a session", {
  cache <- wb_entity_cache(
    path = tempfile(),
    username = "test-user"
  )
  on.exit(unlink(cache$path, recursive = TRUE), add = TRUE)
  
  expect_error(
    wb_get_entity("Q42", cache = cache),
    "Refreshing an authenticated entity cache requires a session"
  )
})

test_that("wb_get_entity preserves managed cache JSON responses", {
  path <- system.file(
    "extdata", "wikidata-cache",
    package = "wikibaser",
    mustWork = TRUE
  )
  
  cache <- wb_entity_cache(
    path = path,
    max_age_hours = Inf
  )
  
  json_file <- wb_get_entity(
    "Q42",
    cache = cache,
    policy = "cache-only",
    as = "json"
  )
  on.exit(unlink(json_file), add = TRUE)
  
  fixture <- file.path(path, "Q42.json")
  
  expect_true(file.exists(json_file))
  expect_false(identical(json_file, fixture))
  
  expect_identical(
    readBin(json_file, "raw", n = file.info(json_file)$size),
    readBin(fixture, "raw", n = file.info(fixture)$size)
  )
})
