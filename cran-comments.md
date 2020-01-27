## Release summary
* Fixes CRAN failures
* Internet based tests are skipped if offline
* All functions that depend on internet resources fail gracefully if resources not available
* Examples that require internet connection are wrapped in \dontrun{}
* removed reshape2 and forcats dependencies

## Test environments
* local ubuntu install, R 3.6.1
* travis (devel and release)
* appveyor (devel and patch)
* win_builder
* rhub

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies

## Additional notes
* Re-release after package was archived on CRAN
