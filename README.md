<link rel="shortcut icon" type="image/x-icon" href="favicon.ico">
# coalitions

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build Status](https://travis-ci.org/adibender/coalitions.svg?branch=master)](https://travis-ci.org/adibender/coalitions)
<!-- [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/adibender/coalitions?branch=master&svg=true)](https://ci.appveyor.com/project/adibender/coalitions) -->
[![Coverage Status](https://codecov.io/github/adibender/coalitions/master.svg)](https://codecov.io/github/adibender/coalitions?branch=master)

## Overview 
The package offers functions that calculate coalition probabilities 
in multi-party elections, based on a survey results. 
It offers two main functions:

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


### Scrape surveys
The wrapper `get_surveys()` which takes no arguments, downloads all surveys currently available at [wahlrecht.de](http://www.wahlrecht.de/umfragen) and stores them in a nested `tibble`: 

```r
surveys <- get_surveys()
surveys
## # A tibble: 6 x 2
##    institute            surveys
##        <chr>             <list>
## 1 allensbach  <tibble [30 x 5]>
## 2      emnid <tibble [185 x 5]>
## 3      forsa <tibble [188 x 5]>
## 4        fgw  <tibble [65 x 5]>
## 5        gms  <tibble [87 x 5]>
## 6  infratest  <tibble [87 x 5]>
```

Each row represents a survey insitute and each row in the `surveys` column again contains a nested `tibble` with survey results from different time-points: 

```r
library(dplyr)
library(tidyr)
surveys %>% 
    filter(institute == "allensbach") %>% 
    unnest()
## # A tibble: 30 x 6
##     institute      datum      start        end befragte           survey
##         <chr>     <date>     <date>     <date>    <dbl>           <list>
##  1 allensbach 2017-04-25 2017-04-01 2017-04-13     1407 <tibble [7 x 3]>
##  2 allensbach 2017-03-28 2017-03-06 2017-03-19     1397 <tibble [7 x 3]>
##  3 allensbach 2017-02-22 2017-02-01 2017-02-15     1542 <tibble [7 x 3]>
##  4 allensbach 2017-01-26 2017-01-05 2017-01-19     1441 <tibble [7 x 3]>
##  5 allensbach 2016-12-22 2016-12-01 2016-12-15     1459 <tibble [7 x 3]>
##  6 allensbach 2016-11-16 2016-10-28 2016-11-10     1436 <tibble [7 x 3]>
##  7 allensbach 2016-10-20 2016-10-01 2016-10-13     1458 <tibble [7 x 3]>
##  8 allensbach 2016-09-22 2016-09-03 2016-09-15     1407 <tibble [7 x 3]>
##  9 allensbach 2016-08-24 2016-08-03 2016-08-17     1496 <tibble [7 x 3]>
## 10 allensbach 2016-07-21 2016-07-01 2016-07-14     1466 <tibble [7 x 3]>
## # ... with 20 more rows

survey <- surveys %>% unnest() %>% slice(1)
survey
# # A tibble: 1 x 6
##    institute      datum      start        end befragte           survey
##        <chr>     <date>     <date>     <date>    <dbl>           <list>
## 1 allensbach 2017-04-25 2017-04-01 2017-04-13     1407 <tibble [7 x 3]>
survey %>% unnest()
## # A tibble: 7 x 8
##    institute      datum      start        end befragte    party percent  votes
##        <chr>     <date>     <date>     <date>    <dbl>    <chr>   <dbl>  <dbl>
## 1 allensbach 2017-04-25 2017-04-01 2017-04-13     1407      cdu      36 506.52
## 2 allensbach 2017-04-25 2017-04-01 2017-04-13     1407      spd      31 436.17
## 3 allensbach 2017-04-25 2017-04-01 2017-04-13     1407   gruene       7  98.49
## 4 allensbach 2017-04-25 2017-04-01 2017-04-13     1407      fdp       6  84.42
## 5 allensbach 2017-04-25 2017-04-01 2017-04-13     1407    linke       9 126.63
## 6 allensbach 2017-04-25 2017-04-01 2017-04-13     1407      afd       7  98.49
## 7 allensbach 2017-04-25 2017-04-01 2017-04-13     1407 sonstige       4  56.28
```

### Calculate coalition probabilities
For each survey (row) we can calculate the coalition probabilities

```r
survey %>% get_probs(nsim=1e4) %>% unnest()
## # A tibble: 6 x 2
##          coalition probability
##              <chr>       <dbl>
## 1              cdu        0.00
## 2          cdu_fdp        0.01
## 3   cdu_fdp_gruene       74.06
## 4              spd        0.00
## 5        linke_spd        0.00
## 6 gruene_linke_spd       22.8
```












