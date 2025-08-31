library(readxl)
library(httr2)
suppressPackageStartupMessages(library(dplyr))
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(tidyr)

# Function to create tempfile and download sheet into it:
download_into_tmpfile <- function(url) {
  # Get the file pattern & extension
  file_pattern_extension <- str_extract(url, ".*/([^/?#]+)(\\.[^./?#]+)(?=$|[?#])", group = c(1, 2))
  pattern <- nth(file_pattern_extension, 1) |>
    URLdecode()

  fileext <- nth(file_pattern_extension, 2)

  # Create the tempfile
  temp_data_file <- tempfile(
    pattern = pattern,
    fileext = fileext
  )
  # Download the file into the tempfile
  download.file(
    url = url,
    destfile = temp_data_file,
    method = "wget",
    extra = c("-U 'Mozilla 5.0'")
  )

  return(temp_data_file)

}

### 2022 NAICS ###

# Getting the full NAICS 2 digit through 6 digit codes
# "https://www.census.gov/naics/2022NAICS/2-6%20digit_2022_Codes.xlsx"
# Use this to get columns:
# year, code, code_title, and code_type
temp_2_6_2022 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/2022NAICS/2-6%20digit_2022_Codes.xlsx"
)

naics_2_6_2022 <- read_excel(
  path = temp_2_6_2022,
  range = "tbl_2022_title_description_coun!A3:C2127",
  col_names = c("seq_no", "code", "code_title"),
  col_types = c("text", "text", "text"),
) |>
  mutate(year = "2022", .before = seq_no) |>
  mutate(
    code_type = case_when(
      nchar(code) == 2L ~ "Sector",
      nchar(code) == 3L ~ "Subsector",
      nchar(code) == 4L ~ "Industry Group",
      nchar(code) == 5L ~ "NAICS Industry",
      nchar(code) == 6L ~ "National Industry",
    )
  ) |>
  select(-seq_no)

# Use these files for code_desc and code_cross_ref columns
# https://www.census.gov/naics/2022NAICS/2022_NAICS_Descriptions.xlsx
# https://www.census.gov/naics/2022NAICS/2022_NAICS_Cross_References.xlsx
temp_cross_ref_2022 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/2022NAICS/2022_NAICS_Cross_References.xlsx"
)

naics_cross_ref_2022 <- read_excel(
  path = temp_cross_ref_2022,
  range = "2022_NAICS_Cross_References!A2:B4602",
  col_names = c("code", "code_cross_ref"),
  col_types = c("text", "text"),
) |>
  group_by(code) |>
  summarize(code_cross_ref = reduce(.x = code_cross_ref, .f = paste))


temp_desc_2022 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/2022NAICS/2022_NAICS_Descriptions.xlsx"
)

naics_desc_2022 <- read_excel(
  path = temp_desc_2022,
  range = "2022_NAICS_Descriptions!A2:C2126",
  col_names = c("code", "code_title", "code_desc"),
  col_types = c("text", "text", "text"),
) |>
  select(-code_title)

# Use this file for the change_desc and trilateral_agreement files
# https://www.census.gov/naics/2022NAICS/2022_NAICS_Structure.xlsx
temp_structure_2022 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/2022NAICS/2022_NAICS_Structure.xlsx"
)

naics_structure_2022 <- read_excel(
  path = temp_structure_2022,
  range = "2022 NAICS Structure!A4:C2147",
  col_names = c("change_ind", "code", "code_title"),
  col_types = c("text", "text", "text"),
) |>
  filter(!is.na(code)) |>
  mutate(
    code = code,
    change_desc = case_when(
      change_ind == "*" ~ "title change, no content change",
      change_ind == "**" ~ "new code for 2022 NAICS",
      change_ind == "***" ~ "re-used code, content change (with or without title change)",
      change_ind == "****" ~ "re-used code, content change at lower level with insignificant impact at this level (with or without title change)",
      T ~ NA_character_
    ),
    trilateral_agreement = if_else(
      str_sub(code_title, -1) == "T", T, F
    ),
    .keep = "none"
  )

naics_2022 <- inner_join(naics_2_6_2022, naics_desc_2022, by = "code") |>
  left_join(naics_cross_ref_2022, by = "code") |>
  inner_join(naics_structure_2022, by = "code")

### 2017 NAICS ###
# Getting the full NAICS 2 digit through 6 digit codes
# https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx
# Use this to get columns:
# year, code, code_title, and code_type
temp_2_6_2017 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx"
)

naics_2_6_2017 <- read_excel(
  path = temp_2_6_2017,
  range = "tbl_2017_title_description_coun!A3:C2198",
  col_names = c("seq_no", "code", "code_title"),
  col_types = c("text", "text", "text"),
) |>
  mutate(year = "2017", .before = seq_no) |>
  mutate(
    code_type = case_when(
      nchar(code) == 2L ~ "Sector",
      nchar(code) == 3L ~ "Subsector",
      nchar(code) == 4L ~ "Industry Group",
      nchar(code) == 5L ~ "NAICS Industry",
      nchar(code) == 6L ~ "National Industry",
    )
  ) |>
  select(-seq_no)

# Use these files for code_desc and code_cross_ref columns
# https://www.census.gov/naics/2017NAICS/2017_NAICS_Descriptions.xlsx
# https://www.census.gov/naics/2017NAICS/2017_NAICS_Cross_References.xlsx
temp_cross_ref_2017 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/2017NAICS/2017_NAICS_Cross_References.xlsx"
)

naics_cross_ref_2017 <- read_excel(
  path = temp_cross_ref_2017,
  range = "2017_NAICS_Cross_References!A2:B4226",
  col_names = c("code", "code_cross_ref"),
  col_types = c("text", "text"),
) |>
  group_by(code) |>
  summarize(code_cross_ref = reduce(.x = code_cross_ref, .f = paste))


temp_desc_2017 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/2017NAICS/2017_NAICS_Descriptions.xlsx"
)

naics_desc_2017 <- read_excel(
  path = temp_desc_2017,
  range = "2017_NAICS_Descriptions!A2:C2197",
  col_names = c("code", "code_title", "code_desc"),
  col_types = c("text", "text", "text"),
) |>
  select(-code_title)

# Use this file for the change_desc and trilateral_agreement files
# https://www.census.gov/naics/2017NAICS/2017_NAICS_Structure.xlsx
temp_structure_2017 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/2017NAICS/2017_NAICS_Structure.xlsx"
)

naics_structure_2017 <- read_excel(
  path = temp_structure_2017,
  range = "Sheet1!A4:C2218",
  col_names = c("change_ind", "code", "code_title"),
  col_types = c("text", "text", "text"),
) |>
  filter(!is.na(code)) |>
  mutate(
    code = code,
    change_desc = case_when(
      change_ind == "*" ~ "title change, no content change",
      change_ind == "**" ~ "new code for 2017 NAICS",
      T ~ NA_character_
    ),
    trilateral_agreement = if_else(
      str_sub(code_title, -1) == "T", T, F
    ),
    .keep = "none"
  )

naics_2017 <- inner_join(naics_2_6_2017, naics_desc_2017, by = "code") |>
  left_join(naics_cross_ref_2017, by = "code") |>
  inner_join(naics_structure_2017, by = "code")

### 2012 NAICS ###
# Getting the full NAICS 2 digit through 6 digit codes
# https://www.census.gov/naics/2012NAICS/2-digit_2012_Codes.xls
# Use this to get columns:
# year, code, code_title, and code_type
temp_2_6_2012 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/2012NAICS/2-digit_2012_Codes.xls"
)

naics_2_6_2012 <- read_excel(
  path = temp_2_6_2012,
  range = "tbl_2012_title_description_coun!A3:C2198",
  col_names = c("seq_no", "code", "code_title"),
  col_types = c("text", "text", "text"),
) |>
  mutate(year = "2012", .before = seq_no) |>
  mutate(
    code_type = case_when(
      nchar(code) == 2L ~ "Sector",
      nchar(code) == 3L ~ "Subsector",
      nchar(code) == 4L ~ "Industry Group",
      nchar(code) == 5L ~ "NAICS Industry",
      nchar(code) == 6L ~ "National Industry",
    )
  ) |>
  select(-seq_no)

# Using the Index file for the code_desc since that's all that's available
temp_idx_2012 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/2012NAICS/2012_NAICS_Index_File.xls"
)

naics_desc_2012 <- read_excel(
  path = temp_idx_2012,
  range = "2012NAICS!A2:B19256",
  col_names = c("code", "code_desc"),
  col_types = c("text", "text"),
) |>
  group_by(code) |>
  summarize(code_desc = reduce(.x = code_desc, .f = paste))

# Filling other fields in with NA since they are missing
naics_2012 <- left_join(naics_2_6_2012, naics_desc_2012, by = "code") |>
  mutate(
    code_cross_ref = NA_character_,
    change_desc = NA_character_,
    trilateral_agreement = NA
  )

### 2007 NAICS ###
# Getting the full NAICS 2 digit through 6 digit codes
# https://www.census.gov/naics/reference_files_tools/2007/naics07.xls
# Use this to get columns:
# year, code, code_title, and code_type
temp_2_6_2007 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/reference_files_tools/2007/naics07.xls"
)

naics_2_6_2007 <- read_excel(
  path = temp_2_6_2007,
  range = "tbl_2007_title_description_coun!A3:C2330",
  col_names = c("seq_no", "code", "code_title"),
  col_types = c("text", "text", "text"),
) |>
  mutate(year = "2007", .before = seq_no) |>
  mutate(
    code_type = case_when(
      nchar(code) == 2L ~ "Sector",
      nchar(code) == 3L ~ "Subsector",
      nchar(code) == 4L ~ "Industry Group",
      nchar(code) == 5L ~ "NAICS Industry",
      nchar(code) == 6L ~ "National Industry",
    )
  ) |>
  select(-seq_no)

# Using the Index file for the code_desc since that's all that's available
temp_idx_2007 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/2007NAICS/2007_NAICS_Index_File.xls"
)

naics_desc_2007 <- read_excel(
  path = temp_idx_2007,
  range = "2007NAICS!A2:B19721",
  col_names = c("code", "code_desc"),
  col_types = c("text", "text"),
) |>
  group_by(code) |>
  summarize(code_desc = reduce(.x = code_desc, .f = paste))

# Filling other fields in with NA since they are missing
naics_2007 <- left_join(naics_2_6_2007, naics_desc_2007, by = "code") |>
  mutate(
    code_cross_ref = NA_character_,
    change_desc = NA_character_,
    trilateral_agreement = NA
  )

### 2002 NAICS ###
# Only the full NAICS 2 digit through 6 digit codes file is available
# https://www.census.gov/naics/reference_files_tools/2002/naics_2_6_02.txt
temp_2_6_2002 <- download_into_tmpfile(
  url = "https://www.census.gov/naics/reference_files_tools/2002/naics_2_6_02.txt"
)

naics_2_6_2002 <- read_lines(
  file = temp_2_6_2002
) |>
  keep(\(x) str_detect(x, "^\\s*\\d{2,6}(?:-\\d{2,6})?\\s+\\S")) |>
  (\(x) {
    tibble(raw = x) |>
      transmute(
        code  = str_match(raw, "^\\s*(\\d{2,6}(?:-\\d{2,6})?)\\s+")[, 2],
        code_title = str_match(raw, "^\\s*\\d{2,6}(?:-\\d{2,6})?\\s+(.+?)\\s*$")[, 2]
      )
  })() |>
  mutate(year = "2002", .before = code) |>
  mutate(
    code_type = case_when(
      nchar(code) == 2L ~ "Sector",
      nchar(code) == 3L ~ "Subsector",
      nchar(code) == 4L ~ "Industry Group",
      nchar(code) == 5L ~ "NAICS Industry",
      nchar(code) == 6L ~ "National Industry",
    )
  )

# Filling all other columns with NAs
naics_2002 <- naics_2_6_2002 |>
  mutate(
    code_desc = NA_character_,
    code_cross_ref = NA_character_,
    change_desc = NA_character_,
    trilateral_agreement = NA
  )

# Filling in the CES and QCEW supersector codes with these resources
# https://www.bls.gov/sae/additional-resources/naics-supersectors-for-ces-program.htm
# https://www.bls.gov/cew/classifications/industry/industry-supersectors.htm

naics_codes <- list_rbind(list(naics_2022, naics_2017, naics_2012, naics_2007, naics_2002)) |>
  mutate(
    code_sector = if_else(code %in% c("31-33", "44-45", "48-49"), code, str_sub(code, 1, 2)),
    case = case_when(
      code_sector %in% c("11", "21") ~ list(tibble(ces_supersector_code = "10", qcew_supersector_code = "1011", ces_domain_code = "06", qcew_domain_code = "101", supersector_title = "Natural Resources and Mining", domain_title = "Goods-Producing")),
      code_sector == "23" ~ list(tibble(ces_supersector_code = "20", qcew_supersector_code = "1012", ces_domain_code = "06", qcew_domain_code = "101", supersector_title = "Construction", domain_title = "Goods-Producing")),
      code_sector %in% c("31", "32", "33", "31-33") ~ list(tibble(ces_supersector_code = "30", qcew_supersector_code = "1013", ces_domain_code = "06", qcew_domain_code = "101", supersector_title = "Manufacturing", domain_title = "Goods-Producing")),
      code_sector %in% c("42", "44", "45", "44-45", "48", "49", "48-49", "22") ~ list(tibble(ces_supersector_code = "40", qcew_supersector_code = "1021", ces_domain_code = "07", qcew_domain_code = "102", supersector_title = "Trade, Transportation, and Utilities", domain_title = "Service-Producing")),
      code_sector == "51" ~ list(tibble(ces_supersector_code = "50", qcew_supersector_code = "1022", ces_domain_code = "07", qcew_domain_code = "102", supersector_title = "Information", domain_title = "Service-Producing")),
      code_sector %in% c("52", "53") ~ list(tibble(ces_supersector_code = "55", qcew_supersector_code = "1023", ces_domain_code = "07", qcew_domain_code = "102", supersector_title = "Financial Activities", domain_title = "Service-Producing")),
      code_sector %in% c("54", "55", "56") ~ list(tibble(ces_supersector_code = "60", qcew_supersector_code = "1024", ces_domain_code = "07", qcew_domain_code = "102", supersector_title = "Professional and Business Services", domain_title = "Service-Producing")),
      code_sector %in% c("61", "62") ~ list(tibble(ces_supersector_code = "65", qcew_supersector_code = "1025", ces_domain_code = "07", qcew_domain_code = "102", supersector_title = "Education and Health Services", domain_title = "Service-Producing")),
      code_sector %in% c("71", "72") ~ list(tibble(ces_supersector_code = "70", qcew_supersector_code = "1026", ces_domain_code = "07", qcew_domain_code = "102", supersector_title = "Leisure and Hospitality", domain_title = "Service-Producing")),
      code_sector == "81" ~ list(tibble(ces_supersector_code = "80", qcew_supersector_code = "1027", ces_domain_code = "07", qcew_domain_code = "102", supersector_title = "Other Services", domain_title = "Service-Producing")),
      code_sector == "92" ~ list(tibble(ces_supersector_code = "90", qcew_supersector_code = "1028", ces_domain_code = "07", qcew_domain_code = "102", supersector_title = "Public Administration", domain_title = "Service-Producing")),
      code_sector == "99" ~ list(tibble(ces_supersector_code = "99", qcew_supersector_code = "1029", ces_domain_code = "07", qcew_domain_code = "102", supersector_title = "Unclassified", domain_title = "Service-Producing"))
    )
  ) |>
  unnest(cols = case) |>
  mutate(across(where(is.character), ~iconv(x = .x, to = "ASCII")))

usethis::use_data(naics_codes, overwrite = TRUE)

# Additional data notes:
# code_type: 2 digit: sector, 3 digit: subsector, 4 digit: industry group, 5 digit: NAICS industry, 6 digit: national industry
# Reference: https://www.bea.gov/help/faq/19
# trilateral_agreement:
# References: https://www.federalregister.gov/documents/2021/12/21/2021-27536/north-american-industry-classification-system-revision-for-2022-update-of-statistical-policy
# and https://www.census.gov/naics/reference_files_tools/2022_NAICS_Manual.pdf and https://unstats.un.org/unsd/classifications/Sprint/Webinar2/Session3_Pres1_Canada.pdf


## BLS NAICS Resources ##
# QCEW industry supersectors: https://www.bls.gov/cew/classifications/industry/industry-supersectors.htm
# Industry classification used by QCEW: https://www.bls.gov/cew/classifications/industry/home.htm
# QCEW industry codes and titles: https://data.bls.gov/cew/doc/titles/industry/industry_titles.htm
# QCEW NAICS hierarchy crosswalk: https://www.bls.gov/cew/classifications/industry/qcew-naics-hierarchy-crosswalk.htm
# BLS & QCEW NAICS differences: https://www.bls.gov/cew/additional-resources/bls-and-qcew-naics-differences.htm
# QCEW industry finder: https://data.bls.gov/cew/apps/bls_naics/v3/bls_naics_app.htm
# Industries by Supersector and NAICS Code: https://www.bls.gov/iag/tgs/iag_index_naics.htm
# CES NAICS Supersectors: https://www.bls.gov/sae/additional-resources/naics-supersectors-for-ces-program.htm


