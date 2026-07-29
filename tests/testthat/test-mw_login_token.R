test_that("mw_login_token() returns a login token", {
  skip_if_no_wikibase()

  req <- httr2::request(Sys.getenv("WIKIBASE_API"))

  token <- mw_login_token(req)

  expect_type(token, "character")
  expect_length(token, 1)
  expect_gt(nchar(token), 10)
})

test_that("mw_login_token() requires an httr2 request", {
  expect_error(
    mw_login_token("not a request")
  )
})
