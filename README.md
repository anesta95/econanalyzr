
<!-- README.md is generated from README.Rmd. Please edit that file -->

# econanalyzr

<!-- badges: start -->

<!-- badges: end -->

The goal of econanalyzr is to provide a set of functions that perform
data transformations that are common when working with economic data.
Functions currently available include:

- `get_bls_data()` which is a wrapper that sends HTTP requests to the
  BLS site with a valid user-supplied email to download TSV data from
  their flat file database.
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

## Data Import Examples

Use the `get_bls_data()` function to get TSV data from the BLS survey
flat file database. This function will require you to provide a valid
email in the request `user-agent` which is now required to
programmatically obtain BLS data in this way.

``` r
library(econanalyzr)
bls_jolts <- get_bls_data(
  bls_url = "https://download.bls.gov/pub/time.series/jt/jt.data.1.AllItems", 
  email = "govdata.decimeter618@passmail.net"
  )

str(bls_jolts)
#> spc_tbl_ [606,736 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#>  $ series_id     : chr [1:606736] "JTS000000000000000HIL" "JTS000000000000000HIL" "JTS000000000000000HIL" "JTS000000000000000HIL" ...
#>  $ year          : chr [1:606736] "2000" "2001" "2001" "2001" ...
#>  $ period        : chr [1:606736] "M12" "M01" "M02" "M03" ...
#>  $ value         : chr [1:606736] "5426" "5722" "5303" "5528" ...
#>  $ footnote_codes: chr [1:606736] NA NA NA NA ...
#>  - attr(*, "spec")=
#>   .. cols(
#>   ..   .default = col_character(),
#>   ..   series_id = col_character(),
#>   ..   year = col_character(),
#>   ..   period = col_character(),
#>   ..   value = col_character(),
#>   ..   footnote_codes = col_character()
#>   .. )
#>  - attr(*, "problems")=<externalptr>
```

## Data Analysis Examples

Use the `percent_change()` function to calculate the percentage change
between two numeric scalars and vectors:

``` r
library(econanalyzr)
# Scalar → scalar
percent_change(100, 120)
#> [1] 0.2

# Vectorized (pairwise)
percent_change(c(100, 80, 50), c(110, 72, 55))
#> [1]  0.1 -0.1  0.1

# Scalar recycling (start is scalar)
percent_change(100, c(101, 105, 120))
#> [1] 0.01 0.05 0.20

# NA propagates (emits a classed warning)
percent_change(c(100, NA), c(110, 120))
#> Warning: NA values detected; returning NA for those positions.
#> [1] 0.1  NA

# Division by zero yields NA (with a single classed warning)
percent_change(c(0, 10), c(5, 15))
#> Warning: Division by zero in `start_value`; returning NA for those positions.
#> [1]  NA 0.5
```

Use the `annualize_change()` function to calculate the annualized *one
month* change between two numeric vectors of values:

``` r
library(econanalyzr)

# Monthly: from 100 to 103 over 1 month (12 months/year)
annualize_change(100, 103, time_elapsed = 1, time_unit = "monthly")
#> [1] 0.4257609

# Vectorized, scalar recycling (quarterly): exponent = 4 / time_elapsed
annualize_change(
  start_values = 100,
  end_values   = c(101, 98, 120),
  time_elapsed = 2,
  time_unit    = "quarterly"
)
#> [1]  0.0201 -0.0396  0.4400

# Daily with default Gregorian year_length = 365.2425
annualize_change(100, 101, time_elapsed = 10, time_unit = "daily")
#> [1] 0.4382518

# Weekly using exact 52-week convention (364 days)
annualize_change(100, 101, time_elapsed = 2, time_unit = "weekly", year_length = 364)
#> [1] 0.2952563
```

Use the `create_index()` to transform a vector of numeric values into a
0 or 100 based index:

``` r
library(econanalyzr)

# Base-100 index (default): base is the first element (50 -> 100)
create_index(c(50, 100, 150))
#> [1] 100 200 300

# Percent-change index (idx_type = 0): base becomes 0%
create_index(c(90, 100, 120), idx_pos = 2, idx_type = 0)
#> [1] -10   0  20

# Select base by *name* (only if the vector is named)
x <- c("2023-01" = 80, "2023-02" = 100, "2023-03" = 120)
create_index(x, idx_pos = "2023-02")            # base-100 with named base
#> 2023-01 2023-02 2023-03 
#>      80     100     120

create_index(x, idx_pos = "2023-02", idx_type = 0)  # percent change from "2023-02"
#> 2023-01 2023-02 2023-03 
#>     -20       0      20

# Non-base NA/NaN/Inf values warn once and propagate
y <- c(100, NA, 200)
create_index(y, idx_pos = 1)
#> Warning: Some non-base values are NA/NaN/Inf; resulting index will contain
#> NA/NaN at those positions.
#> [1] 100  NA 200
```

Use the `create_diffusion_index()` to calculate a Federal Reserve or IHS
diffusion index from numeric values of percent increase, percent
unchanged, or percent decreasing or a Conference Board diffusion index
from percent change values.

``` r
library(econanalyzr)

# Federal Reserve method: (pct_increased − pct_decreased) * 100
create_diffusion_index(
  pct_increased = c(0.60, 0.55, 0.40),
  pct_decreased = c(0.20, 0.25, 0.35),
  idx_type      = "Federal Reserve"
)
#> [1] 40 30  5

# Scalar recycling with FR (scalar increased, vector decreased)
create_diffusion_index(
  pct_increased = 0.50,
  pct_decreased = c(0.20, 0.25, 0.35),
  idx_type      = "Federal Reserve"
)
#> [1] 30 25 15

# IHS-PMI method: (pct_increased + 0.5 * pct_unchanged) * 100
create_diffusion_index(
  pct_increased = c(0.55, 0.50),
  pct_unchanged = c(0.30, 0.35),
  idx_type      = "IHS-PMI"
)
#> [1] 70.0 67.5

# Conference Board method (threshold ±0.05%): encode and average
pc <- c(+0.0010, +0.0005, 0.0000, -0.0005, -0.0020)
create_diffusion_index(pct_change = pc, idx_type = "Conference Board")
#> [1] 50

# Conference Board with NA: NA are dropped from the mean (warns once)
create_diffusion_index(pct_change = c(0.001, NA, -0.001), idx_type = "Conference Board")
#> Warning: NA values found in `pct_change`; they will be dropped when averaging.
#> [1] 50


# All NA example returns NA (single, specific warning)
create_diffusion_index(pct_change = c(NA_real_, NA_real_), idx_type = "Conference Board")
#> Warning: All encoded values are NA; returning NA_real_.
#> [1] NA
```
