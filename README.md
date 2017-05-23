# coalitions

[![Travis-CI Build Status](https://travis-ci.org/adibender/coalitions.svg?branch=master)](https://travis-ci.org/adibender/coalitions)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/adibender/coalitions?branch=master&svg=true)](https://ci.appveyor.com/project/adibender/coalitions)
[![Coverage Status](https://codecov.io/github/adibender/coalitions/master.svg)](https://codecov.io/github/adibender/coalitions?branch=master)

## Overview 
The package offers functions that calculate coalition probabilities 
in multi-party elections, based on a survey results. 
It offerst two main functions:

- `get_surveys`: Scrapes surveys on German Bundestag elections from [wahlrecht.de](http://www.wahlrecht.de)
- `get_probs`: Calculates coalition probabilities based on provided survey results. 

## Installation
Install from GitHub using: 
```r
devtools::install_github("adibender/coalitions", build_vignettes=TRUE)
```


## Usage

Detailed workflow is outlined in the vignettes enclosed in the package, 
especially in `vignette("workflow", package="coalitions")`.













