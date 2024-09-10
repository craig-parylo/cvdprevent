
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CVD Prevent <a href="https://craig-parylo.github.io/cvdprevent/"><img src="man/figures/logo.png" align="right" height="136" alt="cvdprevent website" /></a>

<!-- badges: start -->
<!-- badges: end -->

The goal of CVD Prevent is to provide an R wrapper to the CVD Prevent
[application programming interface](https://www.cvdprevent.nhs.uk/home)
(API). Users can make API requests through built-in R functions.

## Installation

You can install the development version of cvdprevent from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("craig-parylo/cvdprevent")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cvdprevent)

## basic example code
cvd_indicator_list()
```
