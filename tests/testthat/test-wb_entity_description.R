test_that("wb_entity_description returns multilingual metadata", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  result <- wb_entity_description("Q42", cache = cache)

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


test_that("wb_entity_description preserves descriptive languages", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  entity <- wb_get_entity(
    "Q42",
    cache = cache,
    policy = "cache-only"
  )

  result <- wb_entity_description("Q42", cache = cache)

  languages <- unique(c(
    names(entity$labels),
    names(entity$descriptions),
    names(entity$aliases)
  ))

  expect_true(all(result$language %in% languages))

  expect_true(all(
    !is.na(result$label) |
      !is.na(result$description) |
      !is.na(result$alias)
  ))

  expect_true(all(vapply(result, is.character, logical(1))))
})


test_that("wb_entity_description removes completely empty languages", {
  entity <- list(
    id = "Q42",
    labels = list(
      en = list(language = "en", value = "Douglas Adams"),
      hu = list(language = "hu", value = ""),
      nl = list(language = "nl", value = " ")
    ),
    descriptions = list(
      de = list(language = "de", value = "Schriftsteller")
    ),
    aliases = list(
      fr = list(
        list(language = "fr", value = "Douglas Noël Adams"),
        list(language = "fr", value = "DNA")
      )
    )
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL, cache = NULL,
                             policy = "prefer-cache", path = NULL)
      entity
  )

  result <- wb_entity_description("Q42")

  expect_setequal(result$language, c("en", "de", "fr"))

  expect_identical(
    result$alias[result$language == "fr"],
    "Douglas Noël Adams | DNA"
  )

  expect_true(is.na(result$label[result$language == "fr"]))
  expect_true(is.na(result$label[result$language == "de"]))
  expect_true(is.na(result$description[result$language == "en"]))
})


test_that("wb_entity_description handles empty metadata", {
  entity <- list(
    id = "Q42",
    labels = list(),
    descriptions = list(),
    aliases = list()
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL, cache = NULL, ...) entity
  )

  long <- wb_entity_description("Q42")
  wide <- wb_entity_description("Q42", format = "wide")

  expect_s3_class(long, "tbl_df")
  expect_equal(nrow(long), 0L)

  expect_named(
    long,
    c("id", "language", "label", "description", "alias")
  )

  expect_s3_class(wide, "tbl_df")
  expect_equal(nrow(wide), 0L)
  expect_named(wide, "id")
})


test_that("wb_entity_description extracts an equivalent identifier", {
  entity <- list(
    id = "Q42",
    labels = list(
      mul = list(language = "mul", value = "Douglas Adams")
    ),
    claims = list(
      P999999999 = list(
        list(
          rank = "normal",
          mainsnak = list(
            snaktype = "value",
            datavalue = list(
              value = "Q123",
              type = "string"
            )
          )
        )
      )
    )
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL, cache = NULL, ...) entity
  )

  long <- wb_entity_description(
    "Q42",
    equivalency_property = "P999999999"
  )

  wide <- wb_entity_description(
    "Q42",
    equivalency_property = "P999999999",
    format = "wide"
  )

  expect_named(
    long,
    c(
      "id", "equivalent_id", "language",
      "label", "description", "alias"
    )
  )

  expect_identical(long$equivalent_id, "Q123")
  expect_identical(long$id, "Q42")

  expect_named(
    wide,
    c(
      "id", "equivalent_id",
      "label_mul", "description_mul", "alias_mul"
    )
  )

  expect_identical(wide$equivalent_id, "Q123")
  expect_identical(wide$id, "Q42")
  expect_identical(wide$label_mul, "Douglas Adams")
})


test_that("wb_entity_description handles missing equivalency", {
  entity <- list(
    id = "Q42",
    labels = list(
      mul = list(language = "mul", value = "Douglas Adams")
    ),
    claims = list()
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL, cache = NULL, ...) entity
  )

  long <- wb_entity_description(
    "Q42",
    equivalency_property = "P999999999"
  )

  wide <- wb_entity_description(
    "Q42",
    equivalency_property = "P999999999",
    format = "wide"
  )

  expect_identical(long$equivalent_id, NA_character_)
  expect_identical(wide$equivalent_id, NA_character_)
})


test_that("wb_entity_description rejects invalid equivalency properties", {
  invalid_properties <- list(
    "",
    NA_character_,
    "Q42",
    "P0",
    "P01",
    "p31",
    31,
    c("P31", "P279")
  )

  for (property in invalid_properties) {
    expect_error(
      wb_entity_description(
        "Q42",
        equivalency_property = property
      ),
      class = "wikibaser_argument_error"
    )
  }
})


test_that("wb_entity_description returns wide multilingual metadata", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  result <- wb_entity_description(
    "Q42",
    cache = cache,
    format = "wide"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_identical(result$id, "Q42")

  expect_identical(result$label_mul, "Douglas Adams")
  expect_identical(result$label_az, "Duqlas Adams")

  expect_false(any(
    c("language", "label", "description", "alias") %in%
      names(result)
  ))

  expect_true(all(vapply(result, is.character, logical(1))))
})


test_that("wb_entity_description preserves values across formats", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  long <- wb_entity_description(
    "Q42",
    cache = cache,
    format = "long"
  )

  wide <- wb_entity_description(
    "Q42",
    cache = cache,
    format = "wide"
  )

  fields <- c("label", "description", "alias")

  for (field in fields) {
    for (i in seq_len(nrow(long))) {
      column <- paste0(field, "_", long$language[i])

      expect_identical(wide[[column]], long[[field]][i])
    }
  }

  expect_equal(nrow(wide), 1L)
  expect_equal(ncol(wide), 1L + 3L * nrow(long))
})


test_that("wb_entity_description widens bilingual metadata", {
  entity <- list(
    id = "Q1",
    labels = list(
      en = list(language = "en", value = "shirt"),
      hu = list(language = "hu", value = "ing")
    ),
    descriptions = list(
      en = list(language = "en", value = "garment"),
      hu = list(language = "hu", value = "ruhadarab")
    ),
    aliases = list(
      en = list(
        list(language = "en", value = "dress shirt"),
        list(language = "en", value = "formal shirt")
      )
    )
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL, cache = NULL, ...) entity
  )

  result <- wb_entity_description("Q1", format = "wide")

  expected <- tibble::tibble(
    id = "Q1",
    label_en = "shirt",
    label_hu = "ing",
    description_en = "garment",
    description_hu = "ruhadarab",
    alias_en = "dress shirt | formal shirt",
    alias_hu = NA_character_
  )

  expect_identical(result, expected)
})


test_that("wb_entity_description filters languages in long format", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  result <- wb_entity_description(
    "Q42",
    cache = cache,
    languages = c("mul", "az")
  )

  expect_equal(nrow(result), 2L)
  expect_setequal(result$language, c("mul", "az"))

  expect_identical(
    result$label[result$language == "mul"],
    "Douglas Adams"
  )

  expect_identical(
    result$label[result$language == "az"],
    "Duqlas Adams"
  )
})


test_that("wb_entity_description filters languages in wide format", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  result <- wb_entity_description(
    "Q42",
    cache = cache,
    languages = c("mul", "az"),
    format = "wide"
  )

  expect_equal(nrow(result), 1L)
  expect_equal(ncol(result), 7L)

  expect_named(
    result,
    c(
      "id",
      "label_az", "label_mul",
      "description_az", "description_mul",
      "alias_az", "alias_mul"
    )
  )

  expect_identical(result$label_mul, "Douglas Adams")
  expect_identical(result$label_az, "Duqlas Adams")
})


test_that("wb_entity_description handles unavailable languages", {
  entity <- list(
    id = "Q1",
    labels = list(
      en = list(language = "en", value = "shirt"),
      hu = list(language = "hu", value = "ing")
    )
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL, cache = NULL, ...) entity
  )

  long <- wb_entity_description(
    "Q1",
    languages = c("en", "nl")
  )

  wide <- wb_entity_description(
    "Q1",
    languages = c("en", "nl"),
    format = "wide"
  )

  expect_identical(long$language, "en")
  expect_identical(long$label, "shirt")

  expect_named(
    wide,
    c("id", "label_en", "description_en", "alias_en")
  )

  expect_identical(wide$label_en, "shirt")
  expect_identical(wide$description_en, NA_character_)
  expect_identical(wide$alias_en, NA_character_)
})


test_that("wb_entity_description handles unmatched language filters", {
  entity <- list(
    id = "Q1",
    labels = list(
      en = list(language = "en", value = "shirt")
    )
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL, cache = NULL, ...) entity
  )

  long <- wb_entity_description(
    "Q1",
    languages = "hu"
  )

  wide <- wb_entity_description(
    "Q1",
    languages = "hu",
    format = "wide"
  )

  expect_equal(nrow(long), 0L)
  expect_equal(nrow(wide), 0L)

  expect_named(
    long,
    c("id", "language", "label", "description", "alias")
  )

  expect_named(wide, "id")
})


test_that("wb_entity_description handles an empty language vector", {
  entity <- list(
    id = "Q1",
    labels = list(
      en = list(language = "en", value = "shirt")
    )
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL, cache = NULL, ...) entity
  )

  result <- wb_entity_description(
    "Q1",
    languages = character()
  )

  expect_equal(nrow(result), 0L)
})


test_that("wb_entity_description handles repeated language codes", {
  entity <- list(
    id = "Q1",
    labels = list(
      en = list(language = "en", value = "shirt"),
      hu = list(language = "hu", value = "ing")
    )
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL, cache = NULL, ...) entity
  )

  result <- wb_entity_description(
    "Q1",
    languages = c("hu", "en", "en", "hu")
  )

  expect_equal(nrow(result), 2L)
  expect_identical(result$language, c("en", "hu"))
})


test_that("wb_entity_description preserves original language codes", {
  entity <- list(
    id = "Q1",
    labels = list(
      mul = list(language = "mul", value = "Example"),
      "en-gb" = list(language = "en-gb", value = "Colour"),
      "en-us" = list(language = "en-us", value = "Color")
    )
  )

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL, cache = NULL, ...) entity
  )

  result <- wb_entity_description(
    "Q1",
    languages = c("mul", "en-gb", "en-us"),
    format = "wide"
  )

  expect_identical(result$label_mul, "Example")
  expect_identical(result[["label_en-gb"]], "Colour")
  expect_identical(result[["label_en-us"]], "Color")
})


test_that("wb_entity_description defaults to all languages in long format", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  expect_identical(
    wb_entity_description("Q42", cache = cache),
    wb_entity_description(
      "Q42",
      cache = cache,
      format = "long",
      languages = NULL
    )
  )
})


test_that("wb_entity_description rejects invalid language filters", {
  invalid_languages <- list(
    "",
    " ",
    NA_character_,
    c("en", NA_character_),
    c("en", ""),
    42,
    TRUE,
    list("en", "hu")
  )

  for (languages in invalid_languages) {
    expect_error(
      wb_entity_description("Q42", languages = languages),
      class = "wikibaser_argument_error"
    )
  }
})


test_that("wb_entity_description rejects invalid formats", {
  invalid_formats <- list(
    "invalid",
    "",
    NA_character_,
    42
  )

  for (format in invalid_formats) {
    expect_error(
      wb_entity_description("Q42", format = format)
    )
  }
})


test_that("wb_entity_description forwards the entity cache", {
  cache <- wb_entity_cache(
    path = system.file(
      "extdata", "wikidata-cache",
      package = "wikibaser",
      mustWork = TRUE
    ),
    max_age_hours = Inf
  )

  expected_cache <- cache

  testthat::local_mocked_bindings(
    wb_get_entity = function(id, session = NULL, cache = NULL, ...) {
      expect_identical(cache, expected_cache)
      expect_identical(id, "Q42")

      list(
        id = id,
        labels = list(
          mul = list(language = "mul", value = "Douglas Adams")
        )
      )
    }
  )

  result <- wb_entity_description("Q42", cache = cache)

  expect_identical(result$label, "Douglas Adams")
})

test_that("wb_entity_description reads local JSON files", {
  path <- tempfile(tmpdir = tempdir())
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  
  fixture <- system.file(
    "extdata", "wikidata-cache", "Q42.json",
    package = "wikibaser",
    mustWork = TRUE
  )
  
  expect_true(file.copy(fixture, path))
  
  long <- wb_entity_description(
    "Q42",
    path = path,
    languages = c("mul", "az")
  )
  
  wide <- wb_entity_description(
    "Q42",
    path = path,
    languages = c("mul", "az"),
    format = "wide"
  )
  
  expect_equal(nrow(long), 2L)
  expect_equal(nrow(wide), 1L)
  
  expect_identical(wide$label_mul, "Douglas Adams")
  expect_identical(wide$label_az, "Duqlas Adams")
  
  expect_error(
    wb_entity_description("Q999999", path = path),
    class = "wikibaser_entity_file_not_found"
  )
  
  expect_false(file.exists(file.path(path, "cache.rds")))
})
