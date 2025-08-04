
<!-- README.md is generated from README.Rmd. Please edit that file -->

# econanalyzr

<!-- badges: start -->

<!-- badges: end -->

The goal of econanalyzr is to provide a set of functions that perform
data manipulations that are common when working with economic data.
Currently the only functions available are the `percent_change()` which
calculates the percentage change between two numeric scalars or vectors
and `annualize_change()` which calculate the annualized rate of change
between two numeric scalars or vectors for a given time period.

## Installation

You can install the development version of econanalyzr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("anesta95/econanalyzr")
```

## Examples

This is using the `percent_change()` function to calculate the
percentage change between two numeric scalars and vectors:

``` r
library(econanalyzr)
# Basic scalar usage
percent_change(100, 120)
#> [1] 0.2

# Vectorized usage with recycled inputs
percent_change(c(100, 200, 300), 330)
#> [1] 2.30 0.65 0.10

# Handling NA values (produces warning)
percent_change(c(100, NA), c(110, 120))
#> Warning in percent_change(c(100, NA), c(110, 120)): One or more input values
#> are NA; returning NA for those cases.
#> [1] 0.1  NA
```

This is using the `annualize_change()` function to calculate the
annualized *one month* change between two numeric vectors of values:

``` r
library(econanalyzr)

# Example: annualizing monthly changes in GDP
start_vals <- c(100, 105, 110)
end_vals   <- c(102, 107, 112)
time_elapsed <- 1  # 1 month between values

annualize_change(
  start_values = start_vals,
  end_values = end_vals,
  time_elapsed = time_elapsed,
  time_unit = "monthly",
  projection_direction = "up"
)
#> [1] 0.2682418 0.2541045 0.2413780
```
