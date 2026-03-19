## Resubmission (2nd)

This is a second resubmission addressing the feedback from the first review:

- Added `\value` tags to all exported and internal functions that were missing them
- Replaced `\dontrun{}` with `\donttest{}` in examples that require internet
  access but are otherwise executable
- Fixed `scrape_wahlrecht()` example: removed reference to internal object
  via `:::`, replaced with a simple `\donttest` example
- Updated `inst/CITATION` to use `bibentry()` and `c(person())` instead of
  deprecated `citEntry()` and `personList()`
- Fixed redirected URLs in README (codecov, twitter → x.com)
- Removed broken/dead URLs (Travis-CI, AppVeyor, koala.stat.uni-muenchen.de)

## Release summary

This is a resubmission of a previously archived package (archived 2024-04-20).

Changes since last CRAN version (0.6.24):
- Removed deprecated `scrape_austria()` and associated dependencies (RCurl, jsonlite)
- Replaced all deprecated tidyverse functions (`one_of`, `gather`, `mutate_at`,
  `summarize_at`, `filter_at`, `select_if`) with modern equivalents
  (`any_of`, `pivot_longer`, `across`, `if_all`, `where`)
- Fixed documentation typo in `get_superior()` (parameter name `stirng` → `string`)
- Fixed vignettes to use modern tidyverse syntax
- Added BSW party support

## Test environments

* local Ubuntu 24.04, R 4.5.2
* win-builder R-release (R 4.5.2 Patched, 2026-02-13)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies
