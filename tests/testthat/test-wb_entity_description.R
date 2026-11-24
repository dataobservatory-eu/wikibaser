test_that("wb_entity_description returns multilingual metadata", {
  cache <- wb_entity_cache(
    path = testthat::test_path("fixtures", "wikidata-cache"),
    max_age_hours = Inf
  )

  entity <- wb_get_entity(
    "Q42",
    cache = cache,
    policy = "cache-only"
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL) entity
  )

  result <- wb_entity_description("Q42")

  expect_s3_class(result, "tbl_df")

  expect_named(
    result,
    c("id", "language", "label", "description", "alias")
  )

  expect_true(nrow(result) > 0L)
  expect_true(all(result$id == "Q42"))
  expect_identical(anyDuplicated(result$language), 0L)

  expect_identical(
    result$label[result$language == "mul"],
    "Douglas Adams"
  )

  expect_identical(
    result$label[result$language == "az"],
    "Duqlas Adams"
  )
})
