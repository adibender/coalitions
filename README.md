
coalitions
==========

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Travis-CI Build Status](https://travis-ci.org/adibender/coalitions.svg?branch=master)](https://travis-ci.org/adibender/coalitions) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/adibender/coalitions?branch=master&svg=true)](https://ci.appveyor.com/project/adibender/coalitions) [![Coverage Status](https://codecov.io/github/adibender/coalitions/master.svg)](https://codecov.io/github/adibender/coalitions?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/coalitions)](https://cran.r-project.org/package=coalitions) [![MIT license](http://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)

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
    ##   pollster   surveys           
    ##   <chr>      <list>            
    ## 1 allensbach <tibble [38 × 5]> 
    ## 2 emnid      <tibble [212 × 5]>
    ## 3 forsa      <tibble [218 × 5]>
    ## 4 fgw        <tibble [79 × 5]> 
    ## 5 gms        <tibble [93 × 5]> 
    ## 6 infratest  <tibble [102 × 5]>
    ## 7 insa       <tibble [290 × 5]>

Each row represents a survey insitute and each row in the `surveys` column again contains a nested `tibble` with survey results from different time-points:

``` r
surveys %>%
    filter(pollster == "allensbach") %>%
    unnest()
```

    ## # A tibble: 38 x 6
    ##    pollster   date       start      end        respondents survey         
    ##    <chr>      <date>     <date>     <date>           <dbl> <list>         
    ##  1 allensbach 2017-10-25 2017-10-07 2017-10-19        1454 <tibble [7 × 3…
    ##  2 allensbach 2017-09-22 2017-09-13 2017-09-20        1074 <tibble [7 × 3…
    ##  3 allensbach 2017-09-19 2017-09-06 2017-09-14        1083 <tibble [7 × 3…
    ##  4 allensbach 2017-09-06 2017-08-22 2017-08-31        1043 <tibble [7 × 3…
    ##  5 allensbach 2017-08-22 2017-08-04 2017-08-17        1421 <tibble [7 × 3…
    ##  6 allensbach 2017-07-18 2017-07-01 2017-07-12        1403 <tibble [7 × 3…
    ##  7 allensbach 2017-06-20 2017-06-01 2017-06-15        1437 <tibble [7 × 3…
    ##  8 allensbach 2017-05-26 2017-05-05 2017-05-19        1457 <tibble [7 × 3…
    ##  9 allensbach 2017-04-25 2017-04-01 2017-04-13        1407 <tibble [7 × 3…
    ## 10 allensbach 2017-03-28 2017-03-06 2017-03-19        1397 <tibble [7 × 3…
    ## # ... with 28 more rows

``` r
survey <- surveys %>% unnest() %>% slice(1)
survey %>% unnest()
```

    ## # A tibble: 7 x 8
    ##   pollster   date       start      end        responde… party perce… votes
    ##   <chr>      <date>     <date>     <date>         <dbl> <chr>  <dbl> <dbl>
    ## 1 allensbach 2017-10-25 2017-10-07 2017-10-19      1454 cdu    33.0  480  
    ## 2 allensbach 2017-10-25 2017-10-07 2017-10-19      1454 spd    20.5  298  
    ## 3 allensbach 2017-10-25 2017-10-07 2017-10-19      1454 gree…   9.50 138  
    ## 4 allensbach 2017-10-25 2017-10-07 2017-10-19      1454 fdp    12.0  174  
    ## 5 allensbach 2017-10-25 2017-10-07 2017-10-19      1454 left    9.00 131  
    ## 6 allensbach 2017-10-25 2017-10-07 2017-10-19      1454 afd    12.0  174  
    ## 7 allensbach 2017-10-25 2017-10-07 2017-10-19      1454 othe…   4.00  58.2

### Calculate coalition probabilities

For each survey (row) we can calculate the coalition probabilities

``` r
survey %>% get_probabilities(nsim=1e4) %>% unnest()
```

    ## # A tibble: 6 x 4
    ##   pollster   date       coalition       probability
    ##   <chr>      <date>     <chr>                 <dbl>
    ## 1 allensbach 2017-10-25 cdu                   0    
    ## 2 allensbach 2017-10-25 cdu_fdp               0.660
    ## 3 allensbach 2017-10-25 cdu_fdp_greens       99.3  
    ## 4 allensbach 2017-10-25 spd                   0    
    ## 5 allensbach 2017-10-25 left_spd              0    
    ## 6 allensbach 2017-10-25 greens_left_spd       0
