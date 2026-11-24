
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wikibaser <img src="man/figures/logo.png" align="right" height="139" alt="wikibaser logo" />

<!-- badges: start -->

[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![devel-version](https://img.shields.io/badge/devel%20version-0.0.2-blue.svg)](https://github.com/dataobservatory-eu/wikibaser/tree/main)
[![R-CMD-check](https://github.com/dataobservatory-eu/wikibaser/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dataobservatory-eu/wikibaser/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/dataobservatory-eu/wikibaser/graph/badge.svg)](https://app.codecov.io/gh/dataobservatory-eu/wikibaser)
<!-- badges: end -->

**wikibaser** provides a lightweight R interface to the [MediaWiki
Action API](https://www.mediawiki.org/wiki/API:Main_page) for working
with [Wikibase](https://www.mediawiki.org/wiki/Wikibase) repositories
and [Wikidata](https://www.wikidata.org/).

The package supports reproducible retrieval, transformation and
validation of structured, multilingual knowledge from public and
authenticated repositories.

## Features

- Retrieve items and properties through the `wbgetentities` API module.
- Cache original JSON responses for reproducible and offline retrieval.
- Extract multilingual labels, descriptions and aliases into tables.
- Extract statement values and analyse their distribution.
- Authenticate with Wikibase and upload files to MediaWiki.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("dataobservatory-eu/wikibaser")
```

## Example

Retrieve multilingual descriptions of [Q42 (Douglas
Adams)](https://www.wikidata.org/wiki/Q42) using the bundled Wikidata
cache.

``` r
library(wikibaser)

cache <- wb_entity_cache(
  path = system.file(
    "extdata", "wikidata-cache",
    package = "wikibaser"
  ),
  max_age_hours = Inf
)

wb_entity_description(
  "Q42",
  cache = cache,
  languages = c("mul", "az", "en", "hu")
)
#> # A tibble: 4 × 5
#>   id    language label         description                                 alias
#>   <chr> <chr>    <chr>         <chr>                                       <chr>
#> 1 Q42   az       Duqlas Adams  ingilis yazıçı                              Duql…
#> 2 Q42   en       <NA>          British science fiction writer and humoris… <NA> 
#> 3 Q42   hu       <NA>          (1952–2001) angol író és humorista          <NA> 
#> 4 Q42   mul      Douglas Adams <NA>                                        Doug…
```

The example uses a saved API response and does not require a live
connection to Wikidata. The saved entity may differ from its current
Wikidata revision.

See the vignette *Reproducible retrieval of Wikibase entities* for
further examples.

## Development status

**wikibaser** is experimental and under active development. Future
development will focus on entity editing and federation between Wikibase
repositories and Wikidata.

Contributions, bug reports and feature requests are welcome via the
[GitHub issue
tracker](https://github.com/dataobservatory-eu/wikibaser/issues).

``` r
citation("wikibaser")
#> Warning in citation("wikibaser"): could not determine year for 'wikibaser' from
#> package DESCRIPTION file
#> To cite package 'wikibaser' in publications use:
#> 
#>   Antal D (????). _wikibaser: Tools for MediaWiki, Wikibase and
#>   Wikidata Federation_. R package version 0.0.2,
#>   <https://wikibaser.dataobservatory.eu/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {wikibaser: Tools for MediaWiki, Wikibase and Wikidata Federation},
#>     author = {Daniel Antal},
#>     note = {R package version 0.0.2},
#>     url = {https://wikibaser.dataobservatory.eu/},
#>   }
```
