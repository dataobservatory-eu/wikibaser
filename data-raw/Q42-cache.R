## code to prepare `Q42-cache` dataset goes here
## Code to prepare the Q42 cache fixture

devtools::load_all()

fixture_path <- file.path(
  "tests", "testthat", "fixtures", "wikidata-cache"
)

fixture_cache <- wb_entity_cache(
  path = fixture_path,
  max_age_hours = Inf
)

response <- httr2::request(fixture_cache$url) |>
  httr2::req_url_query(
    action = "wbgetentities",
    ids = "Q42",
    format = "json"
  ) |>
  httr2::req_perform()

body <- httr2::resp_body_raw(response)

q42 <- parse_entity_response(body, "Q42")

stopifnot(identical(q42$id, "Q42"))

writeBin(
  body,
  file.path(fixture_cache$path, "Q42.json")
)

stopifnot(
  file.exists(file.path(fixture_cache$path, "Q42.json")),
  file.exists(file.path(fixture_cache$path, "cache.rds"))
)
