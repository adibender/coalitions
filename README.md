
coalitions
==========

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Travis-CI Build Status](https://travis-ci.org/adibender/coalitions.svg?branch=master)](https://travis-ci.org/adibender/coalitions) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/adibender/coalitions?branch=master&svg=true)](https://ci.appveyor.com/project/adibender/coalitions) [![Coverage Status](https://codecov.io/github/adibender/coalitions/master.svg)](https://codecov.io/github/adibender/coalitions?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/coalitions)](https://cran.r-project.org/package=coalitions)

Overview
--------

The package offers functions that calculate coalition probabilities in multi-party elections, based on a survey results. It offers two main functions:

-   `get_surveys`: Scrapes surveys on German Bundestag elections from [wahlrecht.de](http://www.wahlrecht.de)
-   `get_probabilities`: Calculates coalition probabilities based on provided survey results.

Installation
------------

Install from GitHub using:

``` r
devtools::install_github("adibender/coalitions", build_vignettes=TRUE)
```

Usage
-----

Detailed [workflow](https://adibender.github.io/coalitions/articles/workflow.html) is outlined in the vignettes enclosed in the package (see `browseVignettes(package="coalitions")`).

A short overview is presented below.

### Scrape surveys

The wrapper `get_surveys()` which takes no arguments, downloads all surveys currently available at [wahlrecht.de](http://www.wahlrecht.de/umfragen) and stores them in a nested `tibble`:

``` r
library(coalitions)
library(dplyr)
library(tidyr)
surveys <- get_surveys()
surveys
```

    ## # A tibble: 7 x 2
    ##   institute  surveys           
    ##   <chr>      <list>            
    ## 1 allensbach <tibble [34 x 5]> 
    ## 2 emnid      <tibble [200 x 5]>
    ## 3 forsa      <tibble [203 x 5]>
    ## 4 fgw        <tibble [72 x 5]> 
    ## 5 gms        <tibble [90 x 5]> 
    ## 6 infratest  <tibble [95 x 5]> 
    ## 7 insa       <tibble [276 x 5]>

Each row represents a survey insitute and each row in the `surveys` column again contains a nested `tibble` with survey results from different time-points:

``` r
surveys %>% 
    filter(institute == "allensbach") %>% 
    unnest()
```

    ## # A tibble: 34 x 6
    ##    institute  datum      start      end        befragte survey          
    ##    <chr>      <date>     <date>     <date>        <dbl> <list>          
    ##  1 allensbach 2017-08-22 2017-08-04 2017-08-17     1421 <tibble [7 x 3]>
    ##  2 allensbach 2017-07-18 2017-07-01 2017-07-12     1403 <tibble [7 x 3]>
    ##  3 allensbach 2017-06-20 2017-06-01 2017-06-15     1437 <tibble [7 x 3]>
    ##  4 allensbach 2017-05-26 2017-05-05 2017-05-19     1457 <tibble [7 x 3]>
    ##  5 allensbach 2017-04-25 2017-04-01 2017-04-13     1407 <tibble [7 x 3]>
    ##  6 allensbach 2017-03-28 2017-03-06 2017-03-19     1397 <tibble [7 x 3]>
    ##  7 allensbach 2017-02-22 2017-02-01 2017-02-15     1542 <tibble [7 x 3]>
    ##  8 allensbach 2017-01-26 2017-01-05 2017-01-19     1441 <tibble [7 x 3]>
    ##  9 allensbach 2016-12-22 2016-12-01 2016-12-15     1459 <tibble [7 x 3]>
    ## 10 allensbach 2016-11-16 2016-10-28 2016-11-10     1436 <tibble [7 x 3]>
    ## # ... with 24 more rows

``` r
survey <- surveys %>% unnest() %>% slice(1)
survey %>% unnest()
```

    ## # A tibble: 7 x 8
    ##   institute  datum      start      end        befragte party   perc… votes
    ##   <chr>      <date>     <date>     <date>        <dbl> <chr>   <dbl> <dbl>
    ## 1 allensbach 2017-08-22 2017-08-04 2017-08-17     1421 cdu     39.5  561  
    ## 2 allensbach 2017-08-22 2017-08-04 2017-08-17     1421 spd     24.0  341  
    ## 3 allensbach 2017-08-22 2017-08-04 2017-08-17     1421 gruene   7.50 107  
    ## 4 allensbach 2017-08-22 2017-08-04 2017-08-17     1421 fdp     10.0  142  
    ## 5 allensbach 2017-08-22 2017-08-04 2017-08-17     1421 linke    8.00 114  
    ## 6 allensbach 2017-08-22 2017-08-04 2017-08-17     1421 afd      7.00  99.5
    ## 7 allensbach 2017-08-22 2017-08-04 2017-08-17     1421 sonsti…  4.00  56.8

### Calculate coalition probabilities

For each survey (row) we can calculate the coalition probabilities

``` r
survey %>% get_probabilities(nsim=1e4) %>% unnest()
```

    ## # A tibble: 6 x 4
    ##   institute  datum      coalition        probability
    ##   <chr>      <date>     <chr>                  <dbl>
    ## 1 allensbach 2017-08-22 cdu                      0  
    ## 2 allensbach 2017-08-22 cdu_fdp                 86.3
    ## 3 allensbach 2017-08-22 cdu_fdp_gruene          13.7
    ## 4 allensbach 2017-08-22 spd                      0  
    ## 5 allensbach 2017-08-22 linke_spd                0  
    ## 6 allensbach 2017-08-22 gruene_linke_spd         0
