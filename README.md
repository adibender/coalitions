
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
    ## 1 allensbach <tibble [37 x 5]> 
    ## 2 emnid      <tibble [205 x 5]>
    ## 3 forsa      <tibble [210 x 5]>
    ## 4 fgw        <tibble [77 x 5]> 
    ## 5 gms        <tibble [92 x 5]> 
    ## 6 infratest  <tibble [98 x 5]> 
    ## 7 insa       <tibble [281 x 5]>

Each row represents a survey insitute and each row in the `surveys` column again contains a nested `tibble` with survey results from different time-points:

``` r
surveys %>%
    filter(pollster == "allensbach") %>%
    unnest()
```

    ## # A tibble: 37 x 6
    ##    pollster   date       start      end        respondents survey         
    ##    <chr>      <date>     <date>     <date>           <dbl> <list>         
    ##  1 allensbach 2017-09-22 2017-09-13 2017-09-20        1074 <tibble [7 x 3…
    ##  2 allensbach 2017-09-19 2017-09-06 2017-09-14        1083 <tibble [7 x 3…
    ##  3 allensbach 2017-09-06 2017-08-22 2017-08-31        1043 <tibble [7 x 3…
    ##  4 allensbach 2017-08-22 2017-08-04 2017-08-17        1421 <tibble [7 x 3…
    ##  5 allensbach 2017-07-18 2017-07-01 2017-07-12        1403 <tibble [7 x 3…
    ##  6 allensbach 2017-06-20 2017-06-01 2017-06-15        1437 <tibble [7 x 3…
    ##  7 allensbach 2017-05-26 2017-05-05 2017-05-19        1457 <tibble [7 x 3…
    ##  8 allensbach 2017-04-25 2017-04-01 2017-04-13        1407 <tibble [7 x 3…
    ##  9 allensbach 2017-03-28 2017-03-06 2017-03-19        1397 <tibble [7 x 3…
    ## 10 allensbach 2017-02-22 2017-02-01 2017-02-15        1542 <tibble [7 x 3…
    ## # ... with 27 more rows

``` r
survey <- surveys %>% unnest() %>% slice(1)
survey %>% unnest()
```

    ## # A tibble: 7 x 8
    ##   pollster   date       start      end        responde… party perce… votes
    ##   <chr>      <date>     <date>     <date>         <dbl> <chr>  <dbl> <dbl>
    ## 1 allensbach 2017-09-22 2017-09-13 2017-09-20      1074 cdu    36.0  387  
    ## 2 allensbach 2017-09-22 2017-09-13 2017-09-20      1074 spd    22.0  236  
    ## 3 allensbach 2017-09-22 2017-09-13 2017-09-20      1074 gree…   8.00  85.9
    ## 4 allensbach 2017-09-22 2017-09-13 2017-09-20      1074 fdp    10.5  113  
    ## 5 allensbach 2017-09-22 2017-09-13 2017-09-20      1074 left    9.50 102  
    ## 6 allensbach 2017-09-22 2017-09-13 2017-09-20      1074 afd    10.0  107  
    ## 7 allensbach 2017-09-22 2017-09-13 2017-09-20      1074 othe…   4.00  43.0

### Calculate coalition probabilities

For each survey (row) we can calculate the coalition probabilities

``` r
survey %>% get_probabilities(nsim=1e4) %>% unnest()
```

    ## # A tibble: 6 x 4
    ##   pollster   date       coalition       probability
    ##   <chr>      <date>     <chr>                 <dbl>
    ## 1 allensbach 2017-09-22 cdu                     0  
    ## 2 allensbach 2017-09-22 cdu_fdp                13.7
    ## 3 allensbach 2017-09-22 cdu_fdp_greens         86.3
    ## 4 allensbach 2017-09-22 spd                     0  
    ## 5 allensbach 2017-09-22 left_spd                0  
    ## 6 allensbach 2017-09-22 greens_left_spd         0
