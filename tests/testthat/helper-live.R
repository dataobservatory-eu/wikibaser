# Load local developer credentials if available -----------------------------

file_path <- file.path(getwd(), "load_txb_credentials.R")
if (file.exists(file_path)) {
  source(file_path, local = environment())
  message("Sourced")
  load_txb_credentials()
} else {
  message(getwd())
}

# Helper used by live tests -------------------------------------------------

skip_if_no_wikibase <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  testthat::skip_if(
    Sys.getenv("WIKIBASER_RUN_LIVE_TESTS") != "true",
    "Live tests are disabled."
  )

  testthat::skip_if(
    Sys.getenv("WIKIBASE_API") == "",
    "No API endpoint configured."
  )

  testthat::skip_if(
    Sys.getenv("WIKIBASE_USER") == "",
    "No username configured."
  )

  testthat::skip_if(
    Sys.getenv("WIKIBASE_PASSWORD") == "",
    "No password configured."
  )
}
