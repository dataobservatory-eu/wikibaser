test_that("resolve_entity_source defaults to Wikidata", {
  expect_identical(
    resolve_entity_source(NULL, NULL, NULL),
    "https://www.wikidata.org/w/api.php"
  )
})


test_that("resolve_entity_source accepts an explicit API URL", {
  url <- "https://example.org/w/api.php"

  expect_identical(
    resolve_entity_source(NULL, url, NULL),
    url
  )
})


test_that("resolve_entity_source uses the cache URL", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  expect_identical(
    resolve_entity_source(NULL, NULL, cache),
    cache$url
  )

  expect_identical(
    resolve_entity_source(NULL, cache$url, cache),
    cache$url
  )
})


test_that("resolve_entity_source rejects conflicting cache URLs", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  expect_error(
    resolve_entity_source(
      session = NULL,
      url = "https://example.org/w/api.php",
      cache = cache
    ),
    "The API URL does not match the entity cache."
  )
})


test_that("resolve_entity_source uses the session URL", {
  session <- structure(
    list(
      url = "https://example.org/w/api.php",
      username = "test-user"
    ),
    class = c("wikibase_session", "list")
  )

  expect_identical(
    resolve_entity_source(session, NULL, NULL),
    session$url
  )

  expect_identical(
    resolve_entity_source(session, session$url, NULL),
    session$url
  )
})


test_that("resolve_entity_source rejects conflicting session URLs", {
  session <- structure(
    list(
      url = "https://example.org/w/api.php",
      username = "test-user"
    ),
    class = c("wikibase_session", "list")
  )

  expect_error(
    resolve_entity_source(
      session = session,
      url = "https://www.wikidata.org/w/api.php",
      cache = NULL
    ),
    "The API URL does not match the session."
  )
})


test_that("resolve_entity_source accepts matching session and cache", {
  cache <- wb_entity_cache(
    url = "https://example.org/w/api.php",
    username = "test-user"
  )

  on.exit(unlink(cache$path, recursive = TRUE))

  session <- structure(
    list(
      url = cache$url,
      username = cache$username
    ),
    class = c("wikibase_session", "list")
  )

  expect_identical(
    resolve_entity_source(session, NULL, cache),
    cache$url
  )

  expect_identical(
    resolve_entity_source(session, cache$url, cache),
    cache$url
  )
})


test_that("resolve_entity_source rejects different repositories", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  session <- structure(
    list(
      url = "https://example.org/w/api.php",
      username = "test-user"
    ),
    class = c("wikibase_session", "list")
  )

  expect_error(
    resolve_entity_source(session, NULL, cache),
    "The API URL does not match the entity cache."
  )
})


test_that("resolve_entity_source rejects different usernames", {
  cache <- wb_entity_cache(
    url = "https://example.org/w/api.php",
    username = "test-user"
  )

  on.exit(unlink(cache$path, recursive = TRUE))

  session <- structure(
    list(
      url = cache$url,
      username = "another-user"
    ),
    class = c("wikibase_session", "list")
  )

  expect_error(
    resolve_entity_source(session, NULL, cache),
    "The session user does not match the entity cache."
  )
})


test_that("resolve_entity_source rejects authenticated use of public cache", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  session <- structure(
    list(
      url = cache$url,
      username = "test-user"
    ),
    class = c("wikibase_session", "list")
  )

  expect_error(
    resolve_entity_source(session, NULL, cache),
    "The session user does not match the entity cache."
  )
})


test_that("resolve_entity_source accepts offline authenticated caches", {
  cache <- wb_entity_cache(
    url = "https://example.org/w/api.php",
    username = "test-user"
  )

  on.exit(unlink(cache$path, recursive = TRUE))

  expect_identical(
    resolve_entity_source(NULL, NULL, cache),
    cache$url
  )
})


test_that("resolve_entity_source rejects invalid sessions", {
  expect_error(
    resolve_entity_source(list(), NULL, NULL),
    class = "wikibaser_invalid_session"
  )
})


test_that("resolve_entity_source rejects invalid caches", {
  expect_error(
    resolve_entity_source(NULL, NULL, list()),
    class = "wikibaser_invalid_cache"
  )
})
