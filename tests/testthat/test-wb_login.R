# tests/testthat/test-wb_login.R

test_that("wb_login rejects invalid arguments", {
  expect_error(
    wb_login(
      url = "",
      username = "user",
      password = "password"
    ),
    "`url` must be a non-empty character scalar.",
    fixed = TRUE
  )

  expect_error(
    wb_login(
      url = "https://example.org/w/api.php",
      username = "",
      password = "password"
    ),
    "`username` must be a non-empty character scalar.",
    fixed = TRUE
  )

  expect_error(
    wb_login(
      url = "https://example.org/w/api.php",
      username = "user",
      password = ""
    ),
    "`password` must be a non-empty character scalar.",
    fixed = TRUE
  )
})


test_that("wb_login creates an authenticated Wikibase session", {
  skip_on_cran()

  url <- Sys.getenv("WIKIBASE_API")
  username <- Sys.getenv("WIKIBASE_USER")
  password <- Sys.getenv("WIKIBASE_PASSWORD")

  skip_if(
    !nzchar(url) ||
      !nzchar(username) ||
      !nzchar(password),
    "Wikibase credentials are not configured."
  )

  session <- wb_login(
    url = url,
    username = username,
    password = password
  )

  expect_s3_class(session, "wikibase_session")
  expect_type(session, "list")

  expect_named(
    session,
    c(
      "request",
      "csrf_token",
      "username",
      "url",
      "cookie_file"
    )
  )

  expect_s3_class(session$request, "httr2_request")

  expect_type(session$csrf_token, "character")
  expect_length(session$csrf_token, 1L)
  expect_true(nzchar(session$csrf_token))
  expect_false(identical(session$csrf_token, "+\\"))

  expect_identical(session$username, username)
  expect_identical(session$url, url)

  expect_type(session$cookie_file, "character")
  expect_length(session$cookie_file, 1L)
})
