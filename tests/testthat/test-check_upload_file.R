test_that("check_upload_file() accepts an existing file", {
  path <- tempfile(fileext = ".txt")
  writeLines("Hello, Wikibase!", path)

  expect_invisible(
    result <- check_upload_file(path)
  )

  expect_equal(
    result,
    normalizePath(path, winslash = "/", mustWork = TRUE)
  )
})

test_that("check_upload_file() rejects non-character input", {
  expect_error(
    check_upload_file(1),
    class = "wikibaser_invalid_file"
  )

  expect_error(
    check_upload_file(TRUE),
    class = "wikibaser_invalid_file"
  )

  expect_error(
    check_upload_file(character()),
    class = "wikibaser_invalid_file"
  )
})

test_that("check_upload_file() rejects missing values", {
  expect_error(
    check_upload_file(NA_character_),
    class = "wikibaser_invalid_file"
  )
})

test_that("check_upload_file() rejects empty paths", {
  expect_error(
    check_upload_file(""),
    class = "wikibaser_invalid_file"
  )
})

test_that("check_upload_file() rejects non-existent files", {
  path <- tempfile(fileext = ".txt")

  expect_false(file.exists(path))

  expect_error(
    check_upload_file(path),
    class = "wikibaser_file_not_found"
  )
})
