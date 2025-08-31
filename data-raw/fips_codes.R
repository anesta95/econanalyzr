## code to prepare `fips_codes` dataset goes here

# Each line is a geography.

tidycensus_fips <- tidycensus::fips_codes

census_county <- readr::read_delim(
  file = "https://www2.census.gov/geo/docs/reference/codes2020/national_county2020.txt",
  delim = "|",
  col_names = T,
  col_types = readr::cols(.default = readr::col_character())
)






# Resources
# https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html
# https://www.census.gov/programs-surveys/geography/guidance/hierarchy.html
# https://www2.census.gov/geo/pdfs/reference/geodiagram.pdf
# https://www.census.gov/library/reference/code-lists/ansi.html
# https://www.census.gov/programs-surveys/geography/guidance/geo-variants.html
# https://mcdc.missouri.edu/geography/sumlevs/
# https://mcdc.missouri.edu/geography/sumlevs/more-about-sumlevels.html
# https://mcdc.missouri.edu/applications/geocodes/?state=00

usethis::use_data(fips_codes, overwrite = TRUE)
