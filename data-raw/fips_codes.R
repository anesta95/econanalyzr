library(httr2)
library(jsonlite)
suppressPackageStartupMessages(library(dplyr))
library(tibble)
library(janitor)
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
    row_to_names(row_number = 1)

  return(census_resp_df)

}
# data.census.gov query for geographies: https://data.census.gov/table/ACSDT5Y2023.B01003?q=Population+Total&g=010XX00US$0300000,$0400000,$0500000,$1600000,$3100000,$3300000,$7950000_020XX00US1,2,3,4&moe=false
# Census API User Guide: https://www.census.gov/data/developers/guidance/api-user-guide.Ucgid_Predicate.html#list-tab-559651575
# Census GEOID Reference: https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html


### DIVISIONS ###
# https://api.census.gov/data/2023/acs/acs1?get=NAME&ucgid=0200000US1,0200000US2,0200000US3,0200000US4



# Resources
# https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html
# https://www.census.gov/programs-surveys/geography/guidance/hierarchy.html
# https://www2.census.gov/geo/pdfs/reference/geodiagram.pdf
# https://www.census.gov/library/reference/code-lists/ansi.html
# https://www.census.gov/programs-surveys/geography/guidance/geo-variants.html
# https://mcdc.missouri.edu/geography/sumlevs/
# https://mcdc.missouri.edu/geography/sumlevs/more-about-sumlevels.html
# https://mcdc.missouri.edu/applications/geocodes/?state=00
# Census Data API Discovery Tool: https://www.census.gov/data/developers/updates/new-discovery-tool.html

usethis::use_data(fips_codes, overwrite = TRUE)
