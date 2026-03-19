# coalitions 0.6.27 (dev)
- `dHondt()` and `sls()` now return a `ties` attribute (`TRUE`/`FALSE`) indicating
  whether the seat distribution is non-unique due to tied quotients at the last seat.
  When `ties = TRUE`, the result was resolved randomly and re-running may yield a
  different but equally valid distribution (see issue #131).

# coalitions 0.6.26
- Added `\value` documentation to all functions missing return value descriptions
- Updated `inst/CITATION` to use `bibentry()` / `c(person())` (replacing deprecated `citEntry()` / `personList()`)
- Fixed `scrape_wahlrecht()` example to not use internal `:::` accessor
- Replaced `\dontrun{}` with `\donttest{}` in examples requiring internet access
- Fixed redirected and broken URLs in README

# coalitions 0.6.25
- Removed deprecated `scrape_austria()` function and Austria-related dependencies (RCurl, jsonlite)
- Replaced deprecated `one_of()` with `any_of()` throughout
- Replaced deprecated `gather()` with `pivot_longer()`
- Replaced deprecated `mutate_at()`, `summarize_at()`, `filter_at()` with `across()`/`if_all()`
- Replaced deprecated `select_if()` with `where()`
- Fixed documentation typo in `get_superior()` (`stirng` -> `string`)
- Fixed diagnostic vignette (`flatten_df()` -> `bind_rows()`)
- Fixed pooling vignette (bare `unnest()` -> explicit column spec)

# coalitions 0.6.24
Minor test fix

# coalitions 0.6.23
Added/fixed scrappers for German state elections

# coalitions 0.6.20
- fixes CRAN errors (due to change in data structure at wahlrecht.de)

# coalitions 0.6.18
- fix CRAN errors
- fixes error introduced by dependencies (see [#132](https://github.com/adibender/coalitions/pull/132))
- fixed a Bug in `dHondt` function (see [#130](https://github.com/adibender/coalitions/issues/130))

# coalitoins 0.6.15
- fixes CRAN errors

# coaltions 0.6.12
- fixes CRAN errors [#119](https://github.com/adibender/coalitions/issues/119) (see also #118)
- fixes scrapper function for GMS table [#118](https://github.com/adibender/coalitions/issues/118)
- changes `nest`/`unnest` usage to new `tidyr` syntax
- some roxygen fixes

# coalitions 0.6.9
- fixed scrapper for austrian general election [#110](https://github.com/adibender/coalitions/issues/110)
- added more federal election scrappers [#113](https://github.com/adibender/coalitions/issues/113)
- documentation updates

# coalitions 0.6.5
- Fixed scrapper for federal German "Hessen" election
- Fixed tests


# coalitions 0.6.4
- Add scrapers for federal German "Hessen" election

# coalitions 0.6.3
- Update HP with new `pkgdown` version

# coalitions 0.6.2

- Added reference to Description and examples to functions as per CRAN request

# coalitions 0.6.1

- Minor fixes after [JOSS review](https://joss.theoj.org/), thx to Reviewer
[@fsolt](https://github.com/fsolt)
- Updated homepage (via [`pkgdown`](https://pkgdown.r-lib.org/articles/pkgdown.html) )
- Added current JOSS badge

# coalitions 0.6.0 (2018-03)

- Added vignette on [*pooling*](https://adibender.github.io/coalitions/articles/pooling.html)


# coalitions 0.5.7 (2017-12)

- Added federal Bavarian election scrapers

# coalitions 0.5.1 (2017-03)

- Major overhaul
- More robust functions, more tests
- Most functions comply with *tidyverse*
- Improved documentation, added vignettes, created homepage (via `pkgdown`)
