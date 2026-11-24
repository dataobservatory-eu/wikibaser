test_that("count_statements counts individual values", {
  expect_identical(count_statements("Q1"), 1L)
  expect_identical(count_statements("Q1 | Q2"), 2L)
  expect_identical(count_statements("Q1 | Q2 | Q3"), 3L)
})

test_that("count_statements is vectorised", {
  x <- c("Q1", "Q1 | Q2", "Q1 | Q2 | Q3")

  expect_identical(count_statements(x), c(1L, 2L, 3L))
})

test_that("count_statements handles missing and empty values", {
  x <- c(NA_character_, "", " ", "  ", "\t", "\n")

  expect_identical(count_statements(x), rep(0L, 6L))
})

test_that("count_statements handles mixed vectors", {
  x <- c("Q1", NA_character_, "Q2 | Q3", "", "Q4")

  expect_identical(
    count_statements(x),
    c(1L, 0L, 2L, 0L, 1L)
  )
})

test_that("count_statements handles empty vectors", {
  expect_identical(
    count_statements(character(0)),
    integer(0)
  )
})

test_that("count_statements recognises only the exact separator", {
  x <- c("A|B", "A |B", "A| B", "A | B")

  expect_identical(
    count_statements(x),
    c(1L, 1L, 1L, 2L)
  )
})

test_that("count_statements handles different assertion datatypes", {
  x <- c(
    "Q42",
    "P31",
    "Douglas Adams",
    "https://example.org",
    "123456789",
    '{"amount":"+180","unit":"1"}',
    "Cotton | Silk"
  )

  expect_identical(
    count_statements(x),
    c(1L, 1L, 1L, 1L, 1L, 1L, 2L)
  )
})

test_that("count_statements counts repeated values", {
  expect_identical(
    count_statements("Q1 | Q1 | Q2"),
    3L
  )
})
