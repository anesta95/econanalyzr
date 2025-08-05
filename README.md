
<!-- README.md is generated from README.Rmd. Please edit that file -->

# econanalyzr

<!-- badges: start -->

<!-- badges: end -->

The goal of econanalyzr is to provide a set of functions that perform
data manipulations that are common when working with economic data.
Functions currently available include:

- `percent_change()` which calculates the percentage change between two
  numeric scalars or vectors
- `annualize_change()` which calculate the annualized rate of change
  between two numeric scalars or vectors for a given time period.
- `create_index()` which creates a 0 or 100-based index version of a
  numeric vector based on the starting value supplied.

## Installation

You can install the development version of econanalyzr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("anesta95/econanalyzr")
```

## Examples

Use the `percent_change()` function to calculate the percentage change
between two numeric scalars and vectors:

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

Use the `annualize_change()` function to calculate the annualized *one
month* change between two numeric vectors of values:

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

Use the `create_index()` to transform a vector of numeric values into a
0 or 100 based index

``` r
library(econanalyzr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# Basic usage: index centered at 100 using the first value
create_index(c(100, 120, 130))
#> [1] 100 120 130

# Index centered at 0% change
create_index(c(100, 120, 130), idx_type = 0)
#> [1]  0 20 30

# Use a different base position (e.g., second value)
create_index(c(90, 100, 110), idx_pos = 2)
#> [1]  90 100 110

# Use the last value as the base, and index to 0
create_index(c(80, 90, 100), idx_pos = 3, idx_type = 0)
#> [1] -20 -10   0

# Tidyverse integration for time series or grouped workflows
tibble(year = 2020:2023, gdp = c(21000, 22000, 24000, 26000)) %>%
  mutate(gdp_index = create_index(gdp))
#> # A tibble: 4 × 3
#>    year   gdp gdp_index
#>   <int> <dbl>     <dbl>
#> 1  2020 21000      100 
#> 2  2021 22000      105.
#> 3  2022 24000      114.
#> 4  2023 26000      124.
```
