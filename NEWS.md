# wikibaser 0.0.21

### Local entity retrieval

* `wb_get_entity()` and `wb_entity_description()` now accept a
  `path` argument for direct retrieval from local entity JSON files,
  without authentication, cache manifests or API requests.
* Local retrieval preserves the original JSON response and supports
  reproducible offline analysis independently of cache management.
* Added tests for direct local retrieval, byte-for-byte JSON
  preservation, missing files and multilingual metadata extraction.
  
# wikibaser 0.0.2

## Entity retrieval and caching

* Added `wb_get_entity()` to retrieve Wikibase items and properties
  through the `wbgetentities` MediaWiki Action API module.
* Added `wb_entity_cache()` for persistent, repository-specific
  caching of original JSON responses.
* onfigurable cache freshness and retrieval policies,
  including offline retrieval and fallback to cached responses, 
  cache validation.

## Entity descriptions and assertions

* Added `wb_entity_description()` to extract multilingual labels,
  descriptions and aliases into long or wide tables, with optional
  language filtering.
* Added `wb_entity_assertions()` to extract main statement values
  into property columns, with collapsed or widened multiple values.
* Added optional equivalent entity identifiers to support
  cross-repository mapping.
* Added `count_statements()` and `statement_density()` to analyse
  the distribution of statement values across entities and properties.

## Documentation and testing

* Added the vignette "Reproducible retrieval of Wikibase entities".
* Established the pkgdown website, package logo and favicon assets.
* Updated the README and GitHub Actions package-check workflow.

# wikibaser 0.0.1

## Initial development release

* Added authentication to MediaWiki and Wikibase instances with
  `wb_login()`.
* Added file uploads through the MediaWiki Action API with
  `wb_upload_file()`.
* Added internal helpers for session and upload file validation.
* Established the initial package structure and development workflow.

