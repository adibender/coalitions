## Release summary
Added support for regional German election in Hessen

## Test environments
* local ubuntu install, R 3.4.4
* travis (devel and release)
* appveyor (devel and patch)
* win_builder

## R CMD check results

0 errors | 0 warnings | 0 note


## Reverse dependencies

There are no reverse dependencies

## Additional Notes

Adressed "noDL" additional issue by running offending test conditional on capabilities("long.double")
