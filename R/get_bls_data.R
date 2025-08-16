
get_bls_data <- function(url, email) {
  bls_req <- httr2::request(url) |>
    httr2::req_method("GET") |> # This is the default but including to be explicit
    httr2::req_user_agent(string = email) |>
    httr2::req_progress()

  # TODO: How to gracefully handle all types of HTTP errors in this function.
  bls_resp <- httr2::req_perform(bls_req, verbosity = 1)

  # Check if response has a body
  bls_resp |> httr2::resp_has_body()
  # Check if response is correct content type
  bls_resp |> httr2::resp_check_content_type(valid_types = "text/plain")

  # Parse the body as a string
  bls_str_tsv <- httr2::resp_body_string(bls_resp)
  # Convert to R data frame
  bls_df <- readr::read_tsv(
    file = bls_str_tsv,
    col_names = T,
    col_types = readr::cols(.default = readr::col_character()),
    progress = readr::show_progress()
    )


  ##### Old function
  bls_res <- GET(url = url, user_agent(email))
  stop_for_status(bls_res)

  bls_content <- content(bls_res,
                         as = "parsed",
                         type = "text/tab-separated-values",
                         encoding = "UTF-8",
                         col_names = T,
                         col_types = cols(.default = col_character()),
                         trim_ws = T
  )
  return(bls_content)

}

# https://httr2.r-lib.org/articles/wrapping-apis.html
# https://httr2.r-lib.org/reference/index.html
