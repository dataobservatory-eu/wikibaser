test_that("parse_entity_response parses the Q42 fixture", {
  file <- system.file(
    "extdata", "wikidata-cache", "Q42.json",
    package = "wikibaser",
    mustWork = TRUE
  )

  body <- readBin(file, what = "raw", n = file.info(file)$size)
  entity <- parse_entity_response(body, "Q42")

  expect_type(entity, "list")
  expect_identical(entity$id, "Q42")
  expect_identical(entity$type, "item")
  expect_identical(entity$labels$az$value, "Duqlas Adams")
  expect_identical(entity$labels$az$language, "az")
})


test_that("parse_entity_response preserves the entity structure", {
  file <- system.file(
    "extdata", "wikidata-cache", "Q42.json",
    package = "wikibaser",
    mustWork = TRUE
  )

  body <- readBin(file, what = "raw", n = file.info(file)$size)

  response <- jsonlite::fromJSON(
    rawToChar(body),
    simplifyVector = FALSE
  )

  entity <- parse_entity_response(body, "Q42")

  expect_identical(entity, response$entities$Q42)
  expect_type(entity$labels, "list")
  expect_type(entity$descriptions, "list")
  expect_type(entity$aliases, "list")
  expect_type(entity$claims, "list")
  expect_type(entity$sitelinks, "list")
})


test_that("parse_entity_response supports property entities", {
  body <- charToRaw(
    paste0(
      '{"entities":{"P31":{',
      '"id":"P31",',
      '"type":"property",',
      '"datatype":"wikibase-item"',
      "}}}"
    )
  )

  entity <- parse_entity_response(body, "P31")

  expect_type(entity, "list")
  expect_identical(entity$id, "P31")
  expect_identical(entity$type, "property")
  expect_identical(entity$datatype, "wikibase-item")
})


test_that("parse_entity_response rejects API errors", {
  body <- charToRaw(
    paste0(
      '{"error":{',
      '"code":"permissiondenied",',
      '"info":"Access denied"',
      "}}"
    )
  )

  expect_error(
    parse_entity_response(body, "Q42"),
    regexp = "Wikibase API error: Access denied",
    class = "wikibaser_api_error"
  )
})


test_that("parse_entity_response rejects missing entities", {
  body <- charToRaw(
    '{"entities":{"Q42":{"id":"Q42","missing":""}}}'
  )

  expect_error(
    parse_entity_response(body, "Q42"),
    regexp = "was not found",
    class = "wikibaser_entity_not_found"
  )
})


test_that("parse_entity_response rejects absent entity identifiers", {
  file <- system.file(
    "extdata", "wikidata-cache", "Q42.json",
    package = "wikibaser",
    mustWork = TRUE
  )

  body <- readBin(file, what = "raw", n = file.info(file)$size)

  expect_error(
    parse_entity_response(body, "Q5"),
    class = "wikibaser_entity_not_found"
  )
})


test_that("parse_entity_response rejects empty entity collections", {
  body <- charToRaw('{"entities":{}}')

  expect_error(
    parse_entity_response(body, "Q42"),
    class = "wikibaser_entity_not_found"
  )
})


test_that("parse_entity_response rejects absent entity collections", {
  body <- charToRaw('{"success":1}')

  expect_error(
    parse_entity_response(body, "Q42"),
    class = "wikibaser_entity_not_found"
  )
})


test_that("parse_entity_response rejects malformed JSON", {
  expect_error(
    parse_entity_response(charToRaw("not JSON"), "Q42")
  )

  expect_error(
    parse_entity_response(charToRaw('{"entities":'), "Q42")
  )
})


test_that("parse_entity_response rejects invalid response bodies", {
  expect_error(parse_entity_response(raw(), "Q42"))
  expect_error(parse_entity_response(NULL, "Q42"))
  expect_error(parse_entity_response("{}", "Q42"))
})
