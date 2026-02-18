## Release summary
This is a resubmission of a previously archived package.
The package was archived on 2024-04-20.

Changes in this version:
- Removed deprecated `scrape_austria()` and associated dependencies (RCurl, jsonlite)
- Replaced all deprecated tidyverse functions (`one_of`, `gather`, `mutate_at`,
  `summarize_at`, `filter_at`, `select_if`) with modern equivalents
  (`any_of`, `pivot_longer`, `across`, `if_all`, `where`)
- Fixed documentation typo in `get_superior()` (parameter name `stirng` → `string`)
- Fixed vignettes to use modern tidyverse syntax

## Test environments
* local Ubuntu 24.04, R 4.5.2
* (TODO: win_builder, rhub)

## R CMD check results

0 errors | 0 warnings | 0 notes

(Local check shows warnings/notes only from system libxml version mismatch,
not from the package itself.)

## Reverse dependencies

There are no reverse dependencies
