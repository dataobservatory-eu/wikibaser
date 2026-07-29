test_that("mw_login() returns an authenticated request", {
  skip_if_no_wikibase()

  cookie_file <- tempfile("wikibaser-cookies-")
  on.exit(unlink(cookie_file), add = TRUE)

  req <-
    httr2::request(Sys.getenv("WIKIBASE_API")) |>
    httr2::req_cookie_preserve(cookie_file)

  login_token <- mw_login_token(req)

  req <- mw_login(
    req = req,
    username = Sys.getenv("WIKIBASE_USER"),
    password = Sys.getenv("WIKIBASE_PASSWORD"),
    login_token = login_token
  )

  expect_s3_class(req, "httr2_request")

  csrf_token <- mw_csrf_token(req)

  expect_type(csrf_token, "character")
  expect_length(csrf_token, 1)
  expect_gt(nchar(csrf_token), 10)
})


test_that("mw_login() rejects invalid credentials", {
  skip_if_no_wikibase()

  cookie_file <- tempfile("wikibaser-cookies-")
  on.exit(unlink(cookie_file), add = TRUE)

  req <-
    httr2::request(Sys.getenv("WIKIBASE_API")) |>
    httr2::req_cookie_preserve(cookie_file)

  login_token <- mw_login_token(req)

  expect_error(
    mw_login(
      req = req,
      username = Sys.getenv("WIKIBASE_USER"),
      password = "wrong-password",
      login_token = login_token
    ),
    class = "wikibaser_login_error"
  )
})

test_that("mw_login() returns an authenticated request", {
  skip_if_no_wikibase()

  cookie_file <- tempfile("wikibaser-cookies-")
  on.exit(unlink(cookie_file), add = TRUE)

  req <-
    httr2::request(Sys.getenv("WIKIBASE_API")) |>
    httr2::req_cookie_preserve(cookie_file)

  login_token <- mw_login_token(req)

  req <- mw_login(
    req = req,
    username = Sys.getenv("WIKIBASE_USER"),
    password = Sys.getenv("WIKIBASE_PASSWORD"),
    login_token = login_token
  )

  expect_s3_class(req, "httr2_request")

  csrf_token <- mw_csrf_token(req)

  expect_type(csrf_token, "character")
  expect_length(csrf_token, 1)
  expect_gt(nchar(csrf_token), 10)
})


test_that("mw_login() rejects invalid credentials", {
  skip_if_no_wikibase()

  cookie_file <- tempfile("wikibaser-cookies-")
  on.exit(unlink(cookie_file), add = TRUE)

  req <-
    httr2::request(Sys.getenv("WIKIBASE_API")) |>
    httr2::req_cookie_preserve(cookie_file)

  login_token <- mw_login_token(req)

  expect_error(
    mw_login(
      req = req,
      username = Sys.getenv("WIKIBASE_USER"),
      password = "wrong-password",
      login_token = login_token
    ),
    class = "wikibaser_login_error"
  )
})

test_that("mw_login() rejects invalid credentials", {
  skip_if_no_wikibase()

  cookie_file <- tempfile("wikibaser-cookies-")
  on.exit(unlink(cookie_file), add = TRUE)

  req <-
    httr2::request(Sys.getenv("WIKIBASE_API")) |>
    httr2::req_cookie_preserve(cookie_file)

  login_token <- mw_login_token(req)

  expect_error(
    mw_login(
      req = req,
      username = Sys.getenv("WIKIBASE_USER"),
      password = "wrong-password",
      login_token = login_token
    ),
    "Wikibase login failed",
    fixed = TRUE
  )
})
