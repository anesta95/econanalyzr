
<!-- README.md is generated from README.Rmd. Please edit that file -->

# econanalyzr

<!-- badges: start -->

<!-- badges: end -->

The goal of econanalyzr is to provide a collection of functions and
helper datasets that help users obtain, analyze, and visualize economic
data. These functions can be broken up into **data import** functions,
**data analysis** functions, and **data visualization** functions.

These are some of the most commonly used functions currently available:

- `get_bls_data()` which is a wrapper that sends HTTP requests to the
  BLS site with a valid user-supplied email to download TSV data from
  their flat file database.
- `percent_change()` which calculates the percentage change between two
  numeric scalars or vectors.
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
pak::pak("anesta95/econanalyzr")
# or
devtools::install_github("anesta95/econanalyzr")
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
#> spc_tbl_ [607,649 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#>  $ series_id     : chr [1:607649] "JTS000000000000000HIL" "JTS000000000000000HIL" "JTS000000000000000HIL" "JTS000000000000000HIL" ...
#>  $ year          : chr [1:607649] "2000" "2001" "2001" "2001" ...
#>  $ period        : chr [1:607649] "M12" "M01" "M02" "M03" ...
#>  $ value         : chr [1:607649] "5426" "5722" "5303" "5528" ...
#>  $ footnote_codes: chr [1:607649] NA NA NA NA ...
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

## `econanalyzr` Tibble Structure and Functions

Every econ analysis data CSV file at minimum will have the following
columns:

- `date`: The date associated with the data in the data row. The date
  will be in `YYYY-MM-DD` format regardless of the time period the date
  captures. All dates will be the first day of the time period. For
  example, data for April 2025 will be displayed as `2025-04-01`. Data
  for Q2 2025 will be `2025-04-01`. Data for the year 2025 will be
  `2025-01-01`. This will have a data type `double` with a class of
  `Date`.

- `date_period_text`: The time period that each row of the data
  captures. The most common formats are `Monthly`, `Quarterly`, and
  `Annually`. This will have a data type and class of `character`.

- `value`: The value that is being measured in the data. This will have
  a data type of `double` and a class of `numeric`.

- `data_element_text`: What the data in the `value` column describes.
  This will have a data type and class of `character`.

- `data_measure_text`: The mathematical expression the data in the
  `value` column is expressed as. The most common are `Level`, `Rate`,
  `Ratio`, `Percentage`, `Proportion`, and `Index`. This will have a
  data type and class of `character`.

- `date_measure_text`: The change in dates measured by the data in the
  `value` column. The most common are `Current`, `Year-over-year`,
  `Month-over-month` and `Quarter-over-quarter`. This will have a data
  type and class of `character`.

- `data_transform_text`: Any mathematical transformations applied to the
  data. The most common are `Raw`, `Percent change`, `Annualized`,
  `Trail N` where `N` is a number of periods in the `date_period_text`
  column. There can be multiple transformations for each row.
  Transformations are delimited by semi-colons `;` and are stated *in
  order of transformation*. For example, `Trail 3;Percent change` will
  be the percentage change between the trailing 3 period average of the
  current period — denoted in the `date` column — and the trailing 3
  period average of the previous period which is deduced from the
  `date_measure_text`. Conversely, `Percent change;Trail 3` will be the
  trailing 3 period average applied to the percentage change between the
  current period and the previous period across the data series. This
  will have a data type and class of `character`.

- `geo_entity_type_text`: The geographic entity *type* the data in the
  `value` column is covering. This will have a data type and class of
  `character`. If the region is in the United States there is a good
  chance it will be within the [Census Bureau Geographic Entity
  Hierarchy](https://www2.census.gov/geo/pdfs/reference/geodiagram.pdf).

- `geo_entity_text`: The name(s) geographic entity/entities that are
  described by the data.

- `viz_type_text`: The type of visualization made by the data in the
  `value` column. The most common are `Time series line`, `Bar`, `Map`,
  and `Scatter`. This will have a data type and class of `character`.

### Naming conventions

Both CSVs and PNGs are named with the following format where each aspect
of the data is delimited with a dash `-` and spaces are replaced with
underscores `_`.

Data and visualization files will be named in the following order:

1.  `date`
2.  `date_period_text`
3.  `data_element_text`
4.  `data_measure_text`
5.  `date_measure_text`
6.  `data_transform_text`
7.  `geo_entity_type_text`
8.  `geo_entity_text`
9.  *Any other aspects of the data specific to the release that are
    needed to uniquely identify it.* Examples include `industry_text`,
    `size_class_text`, `seas_adj_text`, among others.
10. `viz_type_text`

For data file and chart names with multiple dates in the `date` field —
such as files with a the `viz_type_text` of `Time series line` — the
`date` file naming convention will have the most and least recent dates
in the data series.

#### Examples

- CSV files:
  - `2025-04-01_2023-04-01-monthly-quits-rate-current-2_data_transform-nation-us-total_nonfarm-all_size_classes-seasonally_adjusted-time_series_line.csv`
  - `2025-04-01-monthly-job_openings-rate-year-over-year-trail_3_percent_change-nation-us-12_industry-all_size_classes-seasonally_adjusted-bar.csv`
- PNG files:
  - `2025-04-01_2023-04-01-monthly-quits-rate-current-2_data_transform-nation-us-total_nonfarm-all_size_classes-seasonally_adjusted-time_series_line.png`
  - `2025-04-01-monthly-job_openings-rate-year-over-year-trail_3_percent_change-nation-us-12_industry-all_size_classes-seasonally_adjusted-bar.png`

Every column in the dataset with the `_text` suffix will be included in
the filename, in addition to the `date` column. Data files will also
include columns that have further information that is *not* needed to
uniquely identify the data series. Examples of this include the `value`
column, variables with the `_code` suffix such as `industry_code`,
`fips_code`,`preliminary_code`, as well as `moe`, and `moe_level`, among
others.

## Functions

`econ_value_summary()` filters an econanalyzr-valid data frame by either
a set of dates or a closed date range, pulls a numeric column (defaults
to “value”), and applies a user-supplied function to the filtered
vector.

``` r
library(econanalyzr)

# Minimal econanalyzr-valid example data
df <- tibble::tibble(
  date                 = seq(as.Date("2025-01-01"), by = "month", length.out = 6),
  date_period_text     = "Monthly",
  value                = c(100, 105, 103, 108, 112, 115),  # numeric (double)
  data_element_text    = "Quits Rate",
  data_measure_text    = "Percent",
  date_measure_text    = "Monthly",
  data_transform_text  = "SA",
  geo_entity_type_text = "Nation",
  geo_entity_text      = "US",
  viz_type_text        = "Line"
)

# 1) Inclusive set of dates: mean(value) on Feb & May 2025
econanalyzr::econ_value_summary(
  df,
  dates       = as.Date(c("2025-02-01", "2025-05-01")),
  filter_type = "inclusive",
  .fun        = mean,          # any function or function name is fine
  na_rm       = TRUE           # drop NAs before calling .fun
)
#> [1] 108.5

# 2) Exclusive closed range: median(value) with Jan–Mar 2025 dropped
econanalyzr::econ_value_summary(
  df,
  date_range  = as.Date(c("2025-01-01", "2025-03-31")),
  filter_type = "exclusive",   # drop rows in the range
  .fun        = median
)
#> [1] 112

# 3) Vector-returning function: quantiles over all dates
econanalyzr::econ_value_summary(
  df,
  .fun = function(x) stats::quantile(x, c(.25, .5, .75))  # returns a numeric vector
)
#>   25%   50%   75% 
#> 103.5 106.5 111.0
```

`econ_calc_trail_avg()` computes a right-aligned trailing average of
`value` based on a user-specified window and appends those rows to your
`econanalyzr` tibble in long form. Incomplete windows produce `NA`, and
`; Trail {n}` is appended to the `data_transform_text` field where `n`
is the length of the window.

``` r
library(econanalyzr)

# Minimal econanalyzr-valid data (one series)
df <- tibble::tibble(
  date                 = seq(as.Date("2025-01-01"), by = "month", length.out = 6),
  date_period_text     = "Monthly",
  value                = c(100, 105, 103, 108, 112, 115),  # double
  data_element_text    = "Quits Rate",
  data_measure_text    = "Percent",
  date_measure_text    = "Monthly",
  data_transform_text  = "SA",
  geo_entity_type_text = "Nation",
  geo_entity_text      = "US",
  viz_type_text        = "Line"
)

# Append a 3-period trailing average (right-aligned; first 2 windows are NA)
trail_df <- econanalyzr::econ_calc_trail_avg(df, trail_amount = 3)
# Trailing-average rows are appended; identify them by the suffix in data_transform_text
dplyr::filter(trail_df, grepl("; Trail 3$", data_transform_text)) |>
  dplyr::select(date, value, data_transform_text)
#> # A tibble: 6 × 3
#>   date       value data_transform_text
#>   <date>     <dbl> <chr>              
#> 1 2025-01-01   NA  SA; Trail 3        
#> 2 2025-02-01   NA  SA; Trail 3        
#> 3 2025-03-01  103. SA; Trail 3        
#> 4 2025-04-01  105. SA; Trail 3        
#> 5 2025-05-01  108. SA; Trail 3        
#> 6 2025-06-01  112. SA; Trail 3

# --- Grouped example: compute trailing average within each series ----------------

df_g <- tibble::tibble(
  date                 = rep(seq(as.Date("2025-01-01"), by = "month", length.out = 5), each = 2),
  date_period_text     = "Monthly",
  value                = as.double(c(100, 200, 105, 210, 103, 220, 108, 230, 112, 240)),
  data_element_text    = "Quits Rate",
  data_measure_text    = "Percent",
  date_measure_text    = "Monthly",
  data_transform_text  = "SA",
  geo_entity_type_text = "Nation",
  geo_entity_text      = rep(c("US","CA"), times = 5),
  viz_type_text        = "Line"
)

out_g <- df_g |>
  dplyr::group_by(geo_entity_text) |>
  econanalyzr::econ_calc_trail_avg(3)

# Show trailing-average rows per country (suffix identifies derived rows)
dplyr::filter(out_g, grepl("; Trail 3$", data_transform_text)) |>
  dplyr::select(geo_entity_text, date, value, data_transform_text) |>
  dplyr::arrange(geo_entity_text, date)
#> # A tibble: 10 × 4
#>    geo_entity_text date       value data_transform_text
#>    <chr>           <date>     <dbl> <chr>              
#>  1 CA              2025-01-01   NA  SA; Trail 3        
#>  2 CA              2025-02-01   NA  SA; Trail 3        
#>  3 CA              2025-03-01  210  SA; Trail 3        
#>  4 CA              2025-04-01  220  SA; Trail 3        
#>  5 CA              2025-05-01  230  SA; Trail 3        
#>  6 US              2025-01-01   NA  SA; Trail 3        
#>  7 US              2025-02-01   NA  SA; Trail 3        
#>  8 US              2025-03-01  103. SA; Trail 3        
#>  9 US              2025-04-01  105. SA; Trail 3        
#> 10 US              2025-05-01  108. SA; Trail 3
#> Each country's trailing means are computed independently within its group.
```

`econ_filter_dates()` filters an econanalyzr-valid data frame to a
closed date interval `[start_date, end_date]`. If `start_date` is
omitted, you can specify a period (`period_type` + `period_amount`) and
it derives `start_date = end_date − period` (with `end_date` defaulting
to the latest non-NA date). Returns rows inclusive of the bounds and
ordered newest-first.

``` r
library(econanalyzr)

# Minimal econanalyzr-valid table (10 monthly points)
df <- tibble::tibble(
  date                 = seq(as.Date("2024-09-01"), by = "month", length.out = 10),
  date_period_text     = "Monthly",
  value                = as.double(1:10 * 10),
  data_element_text    = "Quits Rate",
  data_measure_text    = "Percent",
  date_measure_text    = "Monthly",
  data_transform_text  = "SA",
  geo_entity_type_text = "Nation",
  geo_entity_text      = "US",
  viz_type_text        = "Line"
)

# 1) Last 4 months ending at the table’s latest date (derives start_date)
last4 <- econanalyzr::econ_filter_dates(
  df, period_type = "months", period_amount = 4
)
#> Filtered to [2025-02-01, 2025-06-01] (closed interval).
dplyr::select(last4, date, value)
#> # A tibble: 5 × 2
#>   date       value
#>   <date>     <dbl>
#> 1 2025-06-01   100
#> 2 2025-05-01    90
#> 3 2025-04-01    80
#> 4 2025-03-01    70
#> 5 2025-02-01    60


# 2) Between explicit dates (inclusive)
rng <- econanalyzr::econ_filter_dates(
  df,
  start_date = as.Date("2025-02-01"),
  end_date   = as.Date("2025-06-01")
)
#> Filtered to [2025-02-01, 2025-06-01] (closed interval).
dplyr::select(rng, date, value)
#> # A tibble: 5 × 2
#>   date       value
#>   <date>     <dbl>
#> 1 2025-06-01   100
#> 2 2025-05-01    90
#> 3 2025-04-01    80
#> 4 2025-03-01    70
#> 5 2025-02-01    60
```

`econ_csv_write_out()` validates an econanalyzr data frame and writes it
to CSV using a descriptive filename built from the date span and all
\*\_text columns.

``` r
library(econanalyzr)

# Minimal econanalyzr-valid tibble
df <- tibble::tibble(
  date                 = seq(as.Date("2025-01-01"), by = "month", length.out = 6),
  date_period_text     = "Monthly",
  value                = c(100, 105, 103, 108, 112, 115),
  data_element_text    = "Quits Rate",
  data_measure_text    = "Percent",
  date_measure_text    = "Monthly",
  data_transform_text  = "SA",
  geo_entity_type_text = "Nation",
  geo_entity_text      = "US",
  viz_type_text        = "Line"
)

# Write to a temporary folder with a descriptive, sanitized filename
outdir <- tempdir()
path <- econanalyzr::econ_csv_write_out(df, folder = outdir, overwrite = TRUE)
#> Wrote CSV:
#> /tmp/RtmpccYq7b/2025-06-01_2025-01-01-monthly-quits_rate-percent-monthly-sa-nation-us-line.csv

basename(path)
#> [1] "2025-06-01_2025-01-01-monthly-quits_rate-percent-monthly-sa-nation-us-line.csv"

file.exists(path)
#> [1] TRUE
```

## Data Visualization Examples

Economic `{ggplot2}` data visualization templates to come!
