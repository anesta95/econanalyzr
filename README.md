
<!-- README.md is generated from README.Rmd. Please edit that file -->

# econanalyzr

<!-- badges: start -->

<!-- badges: end -->

The goal of econanalyzr is to provide a set of functions that perform
data transformations that are common when working with economic data.
Functions currently available include:

- `percent_change()` which calculates the percentage change between two
  numeric scalars or vectors
- `annualize_change()` which calculate the annualized rate of change
  between two numeric scalars or vectors for a given time period.
- `create_index()` which calculates a 0 or 100-based index version of a
  numeric vector based on the starting value supplied.
- `create_diffusion_index()` which calculates a diffusion index for two
  numeric scalars or vectors based on the [Federal
  Reserve](https://www.richmondfed.org/-/media/richmondfedorg/publications/research/economic_quarterly/2004/fall/pdf/harrisowenssarte.pdf)
  or [IHS
  Markit](https://cdn.ihsmarkit.com/www/pdf/1218/IHS-Markit-PMI-Introduction.pdf)
  methods or one numeric scalar or vector based on the [Conference
  Board](https://www.conference-board.org/data/bci/index.cfm?id=2180)
  method.

## Installation

You can install the development version of econanalyzr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("anesta95/econanalyzr")
```

## Data Analysis Examples

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
0 or 100 based index:

``` r
library(econanalyzr)
suppressPackageStartupMessages(library(dplyr))

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

Use the `create_diffusion_index()` to calculate a Federal Reserve or IHS
diffusion index from numeric values of percent increase, percent
unchanged, or percent decreasing or a Conference Board diffusion index
from percent change values.

``` r
library(econanalyzr)
suppressPackageStartupMessages(library(dplyr))

# --- Federal Reserve method -----------------------------------------------
# Formula: (pct_increased - pct_decreased) * 100
create_diffusion_index(
  pct_increased = c(0.60, 0.50, 0.55),
  pct_decreased = c(0.20, 0.30, 0.25)
)
#> [1] 40 20 30

# Extra args are ignored with a warning
create_diffusion_index(
  pct_increased = 0.60,
  pct_decreased = 0.20,
  pct_unchanged = 0.10  # <- will be ignored with a warning
)
#> Warning in create_diffusion_index(pct_increased = 0.6, pct_decreased = 0.2, :
#> Ignoring 'pct_unchanged' and 'pct_change' for 'Federal Reserve' method.
#> [1] 40

# --- IHS-PMI method --------------------------------------------------------
# Formula: (pct_increased + 0.5 * pct_unchanged) * 100
create_diffusion_index(
  pct_increased = c(0.52, 0.58),
  pct_unchanged = c(0.36, 0.30),
  idx_type = "IHS-PMI"
)
#> [1] 70 73

# Extra args are ignored with a warning
create_diffusion_index(
  pct_increased = 0.55,
  pct_unchanged = 0.30,
  pct_decreased = 0.15, # <- ignored with a warning
  idx_type = "IHS-PMI"
)
#> Warning in create_diffusion_index(pct_increased = 0.55, pct_unchanged = 0.3, :
#> Ignoring 'pct_decreased' and 'pct_change' for 'IHS-PMI' method.
#> [1] 70


# --- Conference Board method -----------------------------------------------
# Encode pct_change per element: > 0.0005 => 1, between +/- 0.0005 => 0.5, < -0.0005 => 0
# Returns the mean of encoded values
create_diffusion_index(
  pct_change = c(0.0010, -0.0020, 0.0002),
  idx_type = "Conference Board"
)
#> [1] 0.5


# --- Tidyverse examples -----------------------------------------------------
# 1) Column-wise calculation in mutate()
tbl <- tibble(
  series = c("A", "A", "A", "B", "B", "B"),
  period = as.Date("2024-01-01") + c(0, 31, 62, 0, 31, 62),
  pct_increased = c(0.56, 0.61, 0.57, 0.48, 0.52, 0.55),
  pct_decreased = c(0.22, 0.19, 0.21, 0.30, 0.28, 0.25),
  pct_unchanged = c(0.22, 0.20, 0.22, 0.22, 0.20, 0.20)
)

tbl %>%
  mutate(
    fed_idx = create_diffusion_index(pct_increased, pct_decreased),
    ihs_idx = create_diffusion_index(pct_increased = pct_increased, pct_unchanged = pct_unchanged, idx_type = "IHS-PMI")
  )
#> # A tibble: 6 × 7
#>   series period     pct_increased pct_decreased pct_unchanged fed_idx ihs_idx
#>   <chr>  <date>             <dbl>         <dbl>         <dbl>   <dbl>   <dbl>
#> 1 A      2024-01-01          0.56          0.22          0.22      34      67
#> 2 A      2024-02-01          0.61          0.19          0.2       42      71
#> 3 A      2024-03-03          0.57          0.21          0.22      36      68
#> 4 B      2024-01-01          0.48          0.3           0.22      18      59
#> 5 B      2024-02-01          0.52          0.28          0.2       24      62
#> 6 B      2024-03-03          0.55          0.25          0.2       30      65

# 2) Grouped summarize for Conference Board
cb_tbl <- tibble(
  industry = rep(c("Goods", "Services"), each = 5),
  pct_change = c(0.001, 0.0004, -0.0008, 0.002, -0.001,
                 0.0003, -0.0006, -0.0012, 0.0015, 0.0002)
)

cb_tbl %>%
  group_by(industry) %>%
  summarise(cb_index = create_diffusion_index(pct_change = pct_change, idx_type = "Conference Board"), .groups = "drop")
#> # A tibble: 2 × 2
#>   industry cb_index
#>   <chr>       <dbl>
#> 1 Goods         0.5
#> 2 Services      0.4
```
