#' North American Industry Classification System Data
#'
#' A reference file of industry and sector data for each NAICS code from 2002 to 2022.
#'
#' @format ## `naics_codes`
#' A data frame with 11,186 rows and 15 columns:
#' \describe{
#'   \item{year}{Year of NAICS version. One of 2022, 2017, 2012, 2007, or 2002.}
#'   \item{code}{The full NAICS code.}
#'   \item{code_title}{A short description of the NAICS code.}
#'   \item{code_type}{The type of NAICS code. One of Sector, Subsector, Industry Group, NAICS Industry, or National Industry. Corresponds to codes with character lengths of 2, 3, 4, 5, and 6, respectively.}
#'   \item{code_desc}{A longer description of what types of businesses and industries are included in the NAICS code.}
#'   \item{code_cross_ref}{Additional descriptive information of what types of businesses and industries are included in the NAICS code.}
#'   \item{change_desc}{Supplementary information if the code title or industries included in the code have changed since the previous version of NAICS.}
#'   \item{trilateral_agreement}{If code is included in the trilateral agreement between Canada, the U.S. and Mexico to be included in each country's industry classification.}
#'   \item{code_sector}{The two-digit sector of the code.}
#'   \item{ces_supersector_code}{The supersector code for the NAICS industry used by the BLS Current Employment Statistics (CES).}
#'   \item{qcew_supersector_code}{The supersector code for the NAICS industry used by the BLS Quarterly Census of Employment and Wages (QCEW).}
#'   \item{ces_domain_code}{The domain (goods-producing vs. service-producing) code used by the BLS Current Employment Statistics (CES).}
#'   \item{qcew_domain_code}{The domain (goods-producing vs. service-producing) code used by the BLS Quarterly Census of Employment and Wages (QCEW).}
#'   \item{supersector_title}{A short description of the supersector for the NAICS code.}
#'   \item{domain_title}{One of Goods-Producing vs. Service-Producing}
#' }
#' @source <https://www.census.gov/naics/?48967>
"naics_codes"

#' US Census Bureau Regions and Divisions Data
#'
#' A reference file of the names, codes, and populations (via the 2025 5-year ACS)
#' of US Census Bureau regions and divisions.
#'
#' @format ## `regions_and_divisions`
#' A data frame with 13 rows and 6 columns:
#' \describe{
#'   \item{geo_id}{The full Census Bureau GEOID of the geography}
#'   \item{geo_entity_text}{The plain text name of the geographic entity.}
#'   \item{pop_acs23_5yr}{The estimated total population of the geography from the B01003 table of the 2023 5-year American Community Survey (ACS).}
#'   \item{sum_lev_code}{The three-digit summary level code to identify the type of Census geography.}
#'   \item{geo_entity_type_text}{The plain text name of the geography level identified in the `sum_lev_code` column.}
#'   \item{geo_code}{The characters after the "US" in the `geo_id` that provide a unique identifier within the specified summary level.}
#' }
#' @source <https://data.census.gov/>
"regions_and_divisions"

#' US Census Bureau States Data
#'
#' A reference file of the names, codes, and populations (via the 2025 5-year ACS)
#' of US Census Bureau States.
#'
#' @format ## `states`
#' A data frame with 52 rows and 9 columns:
#' \describe{
#'   \item{geo_id}{The full Census Bureau GEOID of the geography.}
#'   \item{geo_entity_text}{The plain text name of the geographic entity.}
#'   \item{pop_acs23_5yr}{The estimated total population of the geography from the B01003 table of the 2023 5-year American Community Survey (ACS).}
#'   \item{sum_lev_code}{The three-digit summary level code to identify the type of Census geography.}
#'   \item{geo_entity_type_text}{The plain text name of the geography level identified in the `sum_lev_code` column.}
#'   \item{usps_state_abb}{The two digit state abbreviation used by the USPS to identify the state.}
#'   \item{region_name}{The plain text name of the Census Bureau region that the state is in. More information on regions can be found in the `regions_and_divisions` data set.}
#'   \item{division_name}{The plain text name of the Census Bureau division that the state is in. More information on divisions can be found in the `regions_and_divisions` data set.}
#'   \item{fips_code}{The InterNational Committee for Information Technology Standards (INCITS) formerly known as Federal Information Processing Standards (FIPS) code to ensure uniform identification of geographic entities.}
#' }
#' @source <https://data.census.gov/>
"states"

#' US Census Bureau Counties Data
#'
#' A reference file of the names, codes, and populations (via the 2025 5-year ACS)
#' of US Census Bureau counties and county equivalents.
#'
#' @format ## `counties`
#' A data frame with 3,222 rows and 10 columns:
#' \describe{
#'   \item{geo_id}{The full Census Bureau GEOID of the geography.}
#'   \item{geo_entity_text}{The plain text name of the geographic entity.}
#'   \item{pop_acs23_5yr}{The estimated total population of the geography from the B01003 table of the 2023 5-year American Community Survey (ACS).}
#'   \item{sum_lev_code}{The three-digit summary level code to identify the type of Census geography.}
#'   \item{geo_entity_type_text}{The plain text name of the geography level identified in the `sum_lev_code` column.}
#'   \item{state_fips}{The first two digits of the full county `fips_code` that identify the state that the county is in.}
#'   \item{fips_code}{The InterNational Committee for Information Technology Standards (INCITS) formerly known as Federal Information Processing Standards (FIPS) code to ensure uniform identification of geographic entities.}
#'   \item{fips_class_code}{Identifies what type of county or county equivalent the geography is.}
#'   \item{func_stat_code}{Identifies the functional status of the geography specified by the `fips_code`.}
#'   \item{ns_code}{The National Standard (NS) code used by the US Geological Survey (USGS) for the geography.}
#' }
#' @source <https://data.census.gov/>
"counties"

#' US Census Bureau CBSA Data
#'
#' A reference file of the names, codes, and populations (via the 2025 5-year ACS)
#' of US Census Bureau metropolitan and micropolitan statistical areas that together comprise the core-based statistical areas (CBSAs).
#'
#' @format ## `cbsas`
#' A data frame with 935 rows and 7 columns:
#' \describe{
#'   \item{geo_id}{The full Census Bureau GEOID of the geography.}
#'   \item{geo_entity_text}{The plain text name of the geographic entity.}
#'   \item{pop_acs23_5yr}{The estimated total population of the geography from the B01003 table of the 2023 5-year American Community Survey (ACS).}
#'   \item{sum_lev_code}{The three-digit summary level code to identify the type of Census geography.}
#'   \item{geo_entity_type_text}{The plain text name of the geography level identified in the `sum_lev_code` column.}
#'   \item{geo_code}{The characters after the "US" in the `geo_id` that provide a unique identifier within the specified summary level.}
#'   \item{usps_state_abb}{The two digit state abbreviation used by the USPS to identify the state. This is a list-column with a vector of multiple state abbreviations if the CBSA spans multiple states.}
#' }
#' @source <https://data.census.gov/>
"cbsas"

#' US Census Bureau CSA Data
#'
#' A reference file of the names, codes, and populations (via the 2025 5-year ACS)
#' of US Census Bureau combined statistical areas (CSAs).
#'
#' @format ## `csas`
#' A data frame with 184 rows and 7 columns:
#' \describe{
#'   \item{geo_id}{The full Census Bureau GEOID of the geography.}
#'   \item{geo_entity_text}{The plain text name of the geographic entity.}
#'   \item{pop_acs23_5yr}{The estimated total population of the geography from the B01003 table of the 2023 5-year American Community Survey (ACS).}
#'   \item{sum_lev_code}{The three-digit summary level code to identify the type of Census geography.}
#'   \item{geo_entity_type_text}{The plain text name of the geography level identified in the `sum_lev_code` column.}
#'   \item{geo_code}{The characters after the "US" in the `geo_id` that provide a unique identifier within the specified summary level.}
#'   \item{usps_state_abb}{The two digit state abbreviation used by the USPS to identify the state. This is a list-column with a vector of multiple state abbreviations if the CBSA spans multiple states.}
#' }
#' @source <https://data.census.gov/>
"csas"
