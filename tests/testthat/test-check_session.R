test_that("check_session() accepts a wikibase session", {
  session <- structure(
    list(
      api_url = "https://example.org/w/api.php",
      csrf_token = "123"
    ),
    class = "wikibase_session"
  )

  expect_invisible(
    result <- check_session(session)
  )

  expect_identical(result, session)
})

test_that("check_session() rejects NULL", {
  expect_error(
    check_session(NULL),
    class = "wikibaser_invalid_session"
  )
})

test_that("check_session() rejects arbitrary objects", {
  expect_error(
    check_session(list()),
    class = "wikibaser_invalid_session"
  )

  expect_error(
    check_session("session"),
    class = "wikibaser_invalid_session"
  )

  expect_error(
    check_session(1),
    class = "wikibaser_invalid_session"
  )
})
