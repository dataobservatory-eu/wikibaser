test_that("wb_entity_cache creates a valid temporary cache", {
  cache <- wb_entity_cache()
  on.exit(unlink(cache$path, recursive = TRUE))

  expect_s3_class(cache, "wikibase_entity_cache")
  expect_type(cache, "list")

  expect_named(
    cache,
    c("path", "url", "username", "max_age_hours")
  )

  expect_true(dir.exists(cache$path))
  expect_true(file.exists(file.path(cache$path, "cache.rds")))

  expect_identical(cache$url, "https://www.wikidata.org/w/api.php")
  expect_null(cache$username)
  expect_equal(cache$max_age_hours, 24)

  expect_identical(
    cache$path,
    normalizePath(cache$path, winslash = "/", mustWork = TRUE)
  )
})
