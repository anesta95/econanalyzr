library(httr2)
library(jsonlite)
suppressPackageStartupMessages(library(dplyr))
library(tibble)
suppressPackageStartupMessages(library(janitor))
library(stringr)
suppressPackageStartupMessages(library(purrr))
library(readr)
# Each line is a geography.
# Let's follow this hierarchy from the top down for geographies I'm interested in: https://www2.census.gov/geo/pdfs/reference/geodiagram.pdf
# Simple function to get data from census api
get_census_codes <- function(api_base_url, vars, ucgid) {
  full_vars <- paste(vars, collapse = ",")
  full_api_url <- paste0(api_base_url, full_vars, ucgid)

  census_req <- request(full_api_url) |>
    req_method("GET") |>
    req_verbose() |>
    req_progress()

  census_resp <- req_perform(census_req)

  census_resp_df <- resp_body_json(census_resp, simplifyVector = T) |>
    as_tibble(.name_repair = "unique_quiet") |>
    row_to_names(row_number = 1) |>
    rename(
      geo_entity_text = NAME,
      pop_acs23_5yr = B01003_001E,
      geo_id = ucgid
      ) |>
    mutate(pop_acs23_5yr = as.numeric(pop_acs23_5yr)) |>
    select(geo_id, geo_entity_text, pop_acs23_5yr)

  return(census_resp_df)

}

create_census_sum_lev <- function(df) {
  sumlev_df <- df |>
    mutate(
      sum_lev_code = str_sub(geo_id, 1, 3),
      geo_entity_type_text = case_when(
        sum_lev_code == "020" ~ "Region",
        sum_lev_code == "030" ~ "Division",
        sum_lev_code == "040" ~ "State",
        sum_lev_code == "050" ~ "County",
        sum_lev_code == "160" ~ "Place",
        sum_lev_code == "310" ~ "Core Based Statistical Area",
        sum_lev_code == "330" ~ "Combined Statistical Area"
      )
    )
  return(sumlev_df)
}


# data.census.gov query for geographies: https://data.census.gov/table/ACSDT5Y2023.B01003?q=Population+Total&g=010XX00US$0300000,$0400000,$0500000,$1600000,$3100000,$3300000,$7950000_020XX00US1,2,3,4&moe=false
# Census API User Guide: https://www.census.gov/data/developers/guidance/api-user-guide.Ucgid_Predicate.html#list-tab-559651575
# Census GEOID Reference: https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html
# Summary Level Guides: https://mcdc.missouri.edu/geography/sumlevs/
# https://mcdc.missouri.edu/geography/sumlevs/more-about-sumlevels.html
# Additional county & place info: https://www.census.gov/library/reference/code-lists/ansi.html
# More updated summary levels with different codes for CBSAs and CSAs: https://www.census.gov/programs-surveys/geography/technical-documentation/naming-convention/cartographic-boundary-file/carto-boundary-summary-level.html

## NOTES ##
# Make separate tables! Can do so on the same R file. Do a table for regions & divisions,
# table for counties & places (with extra columns from the additional county/place info flat file)
# and a table for CBSAs (with list-col for states).

## COLUMNS ##
# geo_id
# geo_entity_type_text
# geo_entity_text
# pop_acs23_5yr

## For states counties, places, and CBSAs/CSAs ##
# fips_code

## For counties, places, and CBSAs/CSAs ##
# state_abb
# state_fips

## For counties, places ##
# fips_class_code
# func_stat_code

api_base_url <- "https://api.census.gov/data/2023/acs/acs5?get="
col_vars <- c("NAME", "B01003_001E")

### REGIONS & DIVISIONS ###
# Regions URL https://api.census.gov/data/2023/acs/acs5?get=NAME,B01003_001E&ucgid=0200000US1,0200000US2,0200000US3,0200000US4
# Divisions URL https://api.census.gov/data/2023/acs/acs5?get=NAME,B01003_001E&ucgid=pseudo(0100000US$0300000)
base_regions_df <- get_census_codes(
  api_base_url = api_base_url,
  vars = col_vars,
  ucgid = "&ucgid=0200000US1,0200000US2,0200000US3,0200000US4"
) |>
  create_census_sum_lev()

base_divisions_df <- get_census_codes(
  api_base_url = api_base_url,
  vars = col_vars,
  ucgid = "&ucgid=pseudo(0100000US$0300000)"
) |>
  create_census_sum_lev()

regions_and_divisions <- bind_rows(base_regions_df, base_divisions_df) |>
  mutate(geo_code = str_sub(geo_id, 10, 10))

## STATES ##
# States URL: https://api.census.gov/data/2023/acs/acs5?get=group(B01003)&ucgid=pseudo(0100000US$0400000)
base_states_df <- get_census_codes(
  api_base_url = api_base_url,
  vars = col_vars,
  ucgid = "&ucgid=pseudo(0100000US$0400000)"
) |>
  create_census_sum_lev()

# Joining to get two-character USPS abbreviation
state_name_abb <- tibble(
  geo_entity_text = c(state.name, "District of Columbia", "Puerto Rico"),
  usps_state_abb = c(state.abb, "DC", "PR"),
  region_name = c(as.character(state.region), "South", NA),
  division_name = c(as.character(state.division), "South Atlantic", NA)
)
# Making full states df
states <- inner_join(base_states_df, state_name_abb, by = "geo_entity_text") |>
  mutate(fips_code = str_sub(geo_id, 10, 11))

## COUNTIES ##
# Counties URL: https://api.census.gov/data/2023/acs/acs5?get=group(B01003)&ucgid=pseudo(0100000US$0500000)
base_counties_df <- get_census_codes(
  api_base_url = api_base_url,
  vars = col_vars,
  ucgid = "&ucgid=pseudo(0100000US$0500000)"
) |>
  create_census_sum_lev()

counties_no_func_stat <- base_counties_df |>
  mutate(
    geo_entity_text = str_remove(geo_entity_text, ",.*"),
    state_fips = str_sub(geo_id, 10, 11),
    fips_code = str_sub(geo_id, 10, 14)
    )

# Adding in the extra information on NS codes, fips class code, and functional status codes
# From here: https://www.census.gov/library/reference/code-lists/ansi.html#cou
county_func_stat <- read_delim(
  file = "https://www2.census.gov/geo/docs/reference/codes2020/national_county2020.txt",
  col_names = T,
  col_types = cols(.default = col_character())
) |>
  mutate(fips_code = str_c(STATEFP, COUNTYFP)) |>
  rename(fips_class_code = CLASSFP, func_stat_code = FUNCSTAT, ns_code = COUNTYNS) |>
  select(fips_code, fips_class_code, func_stat_code, ns_code)

counties <- left_join(counties_no_func_stat, county_func_stat, by = "fips_code")

## PLACES ##
# Places URL: https://api.census.gov/data/2023/acs/acs5?get=group(B01003)&ucgid=pseudo(0100000US$1600000)
# NOT USING FOR NOW
base_places_df <- get_census_codes(
  api_base_url = api_base_url,
  vars = col_vars,
  ucgid = "&ucgid=pseudo(0100000US$1600000)"
) |>
  create_census_sum_lev()

places <- base_places_df |>
  mutate(
    geo_entity_text = str_remove(geo_entity_text, ",.*"),
    state_fips = str_sub(geo_id, 10, 11),
    fips_code = str_sub(geo_id, 10, 16)
  )

# Function to extract state abbreviations
get_state_abbs <- function(cbsa_csa_str) {
  state_abbs <- str_extract_all(cbsa_csa_str, "[A-Z]{2}(?:(?:-[A-Z]{2})+)?") |>
    nth(1) |>
    str_split("-") |>
    nth(1)

  return(state_abbs)
}

## CORE-BASED STATISTICAL AREAS (CBSAs) ##
# CBSA URL: https://api.census.gov/data/2023/acs/acs5?get=group(B01003)&ucgid=pseudo(0100000US$3100000)
base_cbsa_df <- get_census_codes(
  api_base_url = api_base_url,
  vars = col_vars,
  ucgid = "&ucgid=pseudo(0100000US$3100000)"
) |>
  create_census_sum_lev()

cbsas <- base_cbsa_df |>
  mutate(
    geo_code = str_sub(geo_id, 10, 14),
    usps_state_abbs = map(geo_entity_text, get_state_abbs)
  )

# COMBINED STATISTICAL AREAS (CSAs)
# CSA URL: https://api.census.gov/data/2023/acs/acs5?get=group(B01003)&ucgid=pseudo(0100000US$3300000)
base_csa_df <- get_census_codes(
  api_base_url = api_base_url,
  vars = col_vars,
  ucgid = "&ucgid=pseudo(0100000US$3300000)"
) |>
  create_census_sum_lev()

csas <- base_csa_df |>
  mutate(
    geo_code = str_sub(geo_id, 10, 12),
    usps_state_abbs = map(geo_entity_text, get_state_abbs)
  )

# Write out data
usethis::use_data(regions_and_divisions, overwrite = TRUE)
usethis::use_data(states, overwrite = TRUE)
usethis::use_data(counties, overwrite = TRUE)
usethis::use_data(cbsas, overwrite = TRUE)
usethis::use_data(csas, overwrite = TRUE)

# Additional Resources
# https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html
# https://www.census.gov/programs-surveys/geography/guidance/hierarchy.html
# https://www2.census.gov/geo/pdfs/reference/geodiagram.pdf
# https://www.census.gov/programs-surveys/geography/guidance/geo-variants.html
# https://mcdc.missouri.edu/applications/geocodes/?state=00
# Census Data API Discovery Tool: https://www.census.gov/data/developers/updates/new-discovery-tool.html
# QCEW Area Code Guide: https://www.bls.gov/cew/classifications/areas/area-guide.htm
