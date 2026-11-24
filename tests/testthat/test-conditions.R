test_that("abort_argument throws the correct classes", {
  expect_error(
    abort_argument("url", "Bad argument"),
    class = "wikibaser_argument_error"
  )

  expect_error(
    abort_argument("url", "Bad argument"),
    class = "wikibaser_error"
  )
})

test_that("abort_login throws the correct classes", {
  expect_error(
    abort_login(),
    class = "wikibaser_login_error"
  )

  expect_error(
    abort_login(),
    class = "wikibaser_error"
  )
})

test_that("abort_http throws the correct classes", {
  expect_error(
    abort_http("HTTP error"),
    class = "wikibaser_http_error"
  )

  expect_error(
    abort_http("HTTP error"),
    class = "wikibaser_error"
  )
})

test_that("abort_api throws the correct classes", {
  expect_error(
    abort_api("API error"),
    class = "wikibaser_api_error"
  )

  expect_error(
    abort_api("API error"),
    class = "wikibaser_error"
  )
})

test_that("abort_invalid_api_url throws the correct classes", {
  expect_error(
    abort_invalid_api_url(),
    class = "wikibaser_api_url_error"
  )

  expect_error(
    abort_invalid_api_url(),
    class = "wikibaser_error"
  )
})


test_that("abort_login formats the error message", {
  expect_snapshot_error(
    abort_login(
      result = "Failed",
      reason = "Incorrect username or password."
    )
  )
})
