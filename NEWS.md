# econanalyzr 1.0.0

* Working on adding included data sets on for additional common economic codes
from the [US Census Bureau](https://www.census.gov/library/reference/code-lists.html).

# econanalyzr 1.0.0

## Major changes

* Added base analysis functions including `percent_change()`, `annualize_change()`,
`create_index()`, and `create_diffusion_index()`.

* Added data retrieval function to get data from [BLS](https://www.bls.gov/) flat file data bases
that will prompt users to supply necessary valid email in `user-agent` HTTPS header.

* Added functions that work with `econanalyzr` tibbles. These include `econ_value_summary()`,
`econ_calc_trail_avg()`, `econ_filter_dates()`, and `econ_csv_write_out()`.

## Minor changes

* Added helper data sets `naics_codes` as a reference file for the 2002-2022 [NAICS](https://www.census.gov/naics/?48967) codes.
As well as the `regions_and_divisions`, `states`, `counties`, `cbsas`, and `csas` files with additional metadata on various common
[US Census Bureau regions](https://www.census.gov/programs-surveys/geography/guidance.html)

