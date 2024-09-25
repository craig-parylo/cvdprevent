
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CVD Prevent <a href="https://craig-parylo.github.io/cvdprevent/"><img src="man/figures/logo.png" align="right" height="136" alt="cvdprevent website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/cvdprevent)](https://CRAN.R-project.org/package=cvdprevent)

[![](https://cranlogs.r-pkg.org/badges/cvdprevent)](https://cran.r-project.org/package=cvdprevent)
<!-- badges: end -->

The goal of CVD Prevent is to provide an R wrapper to the CVD Prevent
[application programming interface](https://www.cvdprevent.nhs.uk/home)
(API). Users can make API requests through built-in R functions.

The Cardiovascular Disease Prevention Audit (CVDPREVENT) is an
England-wide primary care audit that automatically extracts routinely
held GP data. The Data & Improvement Tool provides open access to the
data, with clear, actionable insights for those tasked with improving
cardiovascular health.

## Installation

You can install the development version of cvdprevent from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("craig-parylo/cvdprevent")
```

Alternatively, install the latest stable release from
[CRAN](https://cran.r-project.org/) with:

``` r
utils::install.packages("cvdprevent")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cvdprevent)

## basic example code
cvd_indicator_list() |> 
  head(n = 4)
#> # A tibble: 4 × 10
#>   AxisCharacter DataUpdateInterval FormatDisplayName IndicatorCode
#>   <chr>         <lgl>              <chr>             <chr>        
#> 1 %             NA                 Proportion %      CVDP001AF    
#> 2 %             NA                 Proportion %      CVDP002AF    
#> 3 %             NA                 Proportion %      CVDP001HYP   
#> 4 %             NA                 Proportion %      CVDP004HYP   
#> # ℹ 6 more variables: IndicatorFormatID <int>, IndicatorID <int>,
#> #   IndicatorName <chr>, IndicatorOrder <int>, IndicatorShortName <chr>,
#> #   IndicatorStatus <chr>
```

See `vignette('using_cvdprevent', package = 'cvdprevent')` for more
guidance on use
