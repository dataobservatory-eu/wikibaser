test_that("mw_csrf_token() returns a CSRF token", {
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

  token <- mw_csrf_token(req)

  expect_type(token, "character")
  expect_length(token, 1)
  expect_gt(nchar(token), 10)
})

test_that("mw_csrf_token() requires an httr2 request", {
  expect_error(
    mw_csrf_token("not a request")
  )
})
