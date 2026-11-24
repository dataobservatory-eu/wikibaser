test_that("statement_density counts values by entity and property", {
  assertions <- tibble::tibble(
    id = c("Q1", "Q2", "Q3"),
    P31 = c("Q5 | Q6", "Q5", "Q6"),
    P34 = c("Cotton", NA_character_, "Wool | Silk"),
    P50 = c(NA_character_, NA_character_, "Blue")
  )

  expected <- tibble::tibble(
    id = c("Q1", "Q2", "Q3"),
    P31 = c(2L, 1L, 1L),
    P34 = c(1L, 0L, 2L),
    P50 = c(0L, 0L, 1L)
  )

  expect_identical(statement_density(assertions), expected)
})

test_that("statement_density preserves non-property columns", {
  assertions <- tibble::tibble(
    id = c("Q1", "Q2"),
    equivalent_id = c("Q42", "Q43"),
    label = c("Shirt", "Garment"),
    P31 = c("Q5 | Q6", "Q5")
  )

  result <- statement_density(assertions)

  expect_identical(result$id, assertions$id)
  expect_identical(result$equivalent_id, assertions$equivalent_id)
  expect_identical(result$label, assertions$label)
  expect_identical(result$P31, c(2L, 1L))
  expect_identical(names(result), names(assertions))
})

test_that("statement_density preserves dimensions", {
  assertions <- tibble::tibble(
    id = c("Q1", "Q2"),
    P31 = c("Q5", "Q6"),
    P34 = c("Cotton | Silk", NA_character_)
  )

  result <- statement_density(assertions)

  expect_s3_class(result, "tbl_df")
  expect_identical(dim(result), dim(assertions))
  expect_identical(names(result), names(assertions))
})

test_that("statement_density handles missing and empty values", {
  assertions <- tibble::tibble(
    id = c("Q1", "Q2", "Q3", "Q4"),
    P31 = c(NA_character_, "", " ", "Q5")
  )

  result <- statement_density(assertions)

  expect_identical(result$P31, c(0L, 0L, 0L, 1L))
})

test_that("statement_density handles entities without properties", {
  assertions <- tibble::tibble(
    id = c("Q1", "Q2"),
    equivalent_id = c("Q42", "Q43")
  )

  expect_identical(statement_density(assertions), assertions)
})

test_that("statement_density handles zero-row tibbles", {
  assertions <- tibble::tibble(
    id = character(0),
    P31 = character(0),
    P34 = character(0)
  )

  result <- statement_density(assertions)

  expect_equal(nrow(result), 0L)
  expect_identical(names(result), names(assertions))
  expect_identical(result$P31, integer(0))
  expect_identical(result$P34, integer(0))
})

test_that("statement_density accepts base data frames", {
  assertions <- data.frame(
    id = c("Q1", "Q2"),
    P31 = c("Q5 | Q6", "Q5"),
    stringsAsFactors = FALSE
  )

  result <- statement_density(assertions)

  expect_s3_class(result, "tbl_df")
  expect_identical(result$P31, c(2L, 1L))
})

test_that("statement_density recognises only valid property columns", {
  assertions <- tibble::tibble(
    id = "Q1",
    P31 = "Q5 | Q6",
    P0 = "A | B",
    P01 = "A | B",
    P31_1 = "Q5",
    P31_2 = "Q6",
    Q42 = "A | B",
    label = "A | B"
  )

  expected <- assertions
  expected$P31 <- 2L

  expect_identical(statement_density(assertions), expected)
})

test_that("statement_density supports aggregate density calculations", {
  assertions <- tibble::tibble(
    id = c("Q1", "Q2", "Q3"),
    P31 = c("Q5 | Q6", "Q5", "Q6"),
    P34 = c("Cotton", NA_character_, "Wool | Silk"),
    P50 = c(NA_character_, NA_character_, "Blue")
  )

  density <- statement_density(assertions)
  m <- as.matrix(density[c("P31", "P34", "P50")])

  expect_equal(sum(m), 8)
  expect_equal(sum(m > 0), 6)
  expect_equal(sum(m == 0), 3)

  expect_equal(mean(m > 0), 2 / 3)
  expect_equal(mean(m == 0), 1 / 3)

  expect_equal(unname(rowSums(m)), c(3, 1, 4))
  expect_equal(unname(colSums(m)), c(4, 3, 1))
  expect_equal(max(m), 2)
})

test_that("statement_density does not modify the original assertions", {
  assertions <- tibble::tibble(
    id = "Q1",
    P31 = "Q5 | Q6"
  )

  result <- statement_density(assertions)

  expect_identical(assertions$P31, "Q5 | Q6")
  expect_identical(result$P31, 2L)
})
