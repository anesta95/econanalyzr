#' Fetch a TSV from the BLS flat file database over HTTP with an email User-Agent and parse it
#'
#' @description
#' Validates `bls_url` and `email` as string scalars, verifies the email format,
#' sends a GET request using **httr2** with the email in the User-Agent header
#' and a request timeout, and on a successful response with `Content-Type: text/plain`
#' or `text/tab-separated-values` and a non-empty body, parses the payload as TSV
#' via **readr** and returns a tibble.
#'
#' @param bls_url A string scalar BLS URL (e.g., "https://download.bls.gov/pub/time.series/jt/jt.data.1.AllItems").
#' @param email A string scalar email address to include in the User-Agent header.
#'
#' @return A tibble parsed from the TSV response body.
#' @details
#' - Email validation uses a practical regex rather than full RFC 5322.
#' - HTTP errors (non-2xx) raise an error with status, reason, URL, response headers,
#'   and a short body snippet (best-effort).
#' - If the response is successful but not `text/plain` or `text/tab-separated-values`,
#'   or the body is empty, an error is raised (with headers included).
#'
#' @examples
#' \dontrun{
#' df <- get_bls_data(
#'   bls_url = "https://download.bls.gov/pub/time.series/jt/jt.data.1.AllItems",
#'   email   = "you@example.com"
#' )
#' }
#'
#' @export
#' @importFrom httr2 request req_user_agent req_timeout req_perform req_progress req_error
#' @importFrom httr2 resp_status resp_status_desc resp_content_type resp_body_string resp_headers resp_url resp_has_body
#' @importFrom readr read_tsv cols col_character show_progress
#' @importFrom rlang abort is_string
#' @importFrom glue glue
get_bls_data <- function(bls_url, email) {
  ### Small helpers ###
  # Structured error for non-scalar or missing string inputs.
  abort_bad_scalar <- function(arg, cls = "get_bls_data_bad_scalar") {
    rlang::abort(
      message = glue::glue("`{arg}` must be a non-missing string scalar."),
      class   = cls,
      arg     = arg
    )
  }

  # Minimal scalar string check: single, non-NA character vector.
  is_string_scalar <- function(x) {
    rlang::is_string(x) && !is.na(x)
  }

  # Practical email pattern: user@domain.tld (not full RFC 5322).
  is_valid_email <- function(x) {
    grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", x, perl = TRUE)
  }

  ### Validate inputs ###
  if (!is_string_scalar(bls_url)) abort_bad_scalar("bls_url")
  if (!is_string_scalar(email))   abort_bad_scalar("email")

  # Expect http/https (avoid odd schemes like file:, ftp:, etc.)
  if (!grepl("^https?://", bls_url)) {
    rlang::abort(
      message = glue::glue("The `bls_url` argument must begin with 'http://' or 'https://'."),
      class   = "get_bls_data_bad_url",
      bls_url = bls_url
    )
  }

  # Require a plausibly valid email; many public data providers request this.
  if (!is_valid_email(email)) {
    rlang::abort(
      message = glue::glue("The `email` argument is not a valid email address."),
      class   = "get_bls_data_bad_email",
      email   = email
    )
  }

  ### Build request ###
  # Compose a descriptive UA including your package + contact email.
  ua <- glue::glue("econanalyzr/0.0.0.9000 (+mailto:{email})")

  # Add a timeout to avoid hanging forever (tune seconds as desired).
  req <- httr2::request(bls_url) |>
    httr2::req_user_agent(ua) |>
    httr2::req_progress() |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = ~ F)

  ### Perform request with robust error reporting ###
  # Catch transport-level failures (DNS/TLS/timeouts/connection resets/etc.).
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(cnd) {
      rlang::abort(
        message = "Request failed before receiving an HTTP response.",
        class   = "get_bls_data_transport_error",
        parent  = cnd,          # keep original condition for debugging
        bls_url = bls_url
        # no headers available here because we never received a response
      )
    }
  )

  ### HTTP status handling ###
  status <- httr2::resp_status(resp)
  if (status < 200L || status >= 300L) {
    reason <- httr2::resp_status_desc(resp)

    # Try to include up to 500 chars of body to aid debugging (ignore decode errors).
    # Only try to read a snippet if the response actually has a body.
    body_snippet <- if (httr2::resp_has_body(resp)) {
      tryCatch({
        raw <- httr2::resp_body_string(resp)
        raw <- trimws(raw)
        if (nzchar(raw)) substr(raw, 1L, 500L) else ""
      }, error = function(e) "")
    } else {
      ""
    }

    rlang::abort(
      message = glue::glue("HTTP error {status} ({reason}) from {httr2::resp_url(resp)}."),
      class   = "get_bls_data_http_error",
      bls_url = httr2::resp_url(resp),
      status  = status,
      reason  = reason,
      headers = httr2::resp_headers(resp),  # include response headers
      body_snippet = body_snippet
    )
  }

  ### Check body presence BEFORE content-type/parse ###
  # HEAD/1xx/204/304 (rare for GET) might have no body; bail early with context.
  if (!httr2::resp_has_body(resp)) {
    rlang::abort(
      message = "Response contains no body; expected a TSV payload.",
      class   = "get_bls_data_no_body",
      bls_url = httr2::resp_url(resp),
      headers = httr2::resp_headers(resp)
    )
  }

  ### Check content-type & body ###
  # Accept the two common TSV types used by BLS and similar services.
  ctype <- httr2::resp_content_type(resp)
  if (!ctype %in% c("text/plain", "text/tab-separated-values")) {
    rlang::abort(
      message = glue::glue(
        "Expected Content-Type 'text/plain' or 'text/tab-separated-values' but got '{ctype}' from {httr2::resp_url(resp)}."
      ),
      class   = "get_bls_data_bad_content_type",
      bls_url = httr2::resp_url(resp),
      ctype   = ctype,
      headers = httr2::resp_headers(resp)
    )
  }

  # Get the response body as a string (httr2 decodes per charset when present).
  bls_str_tsv <- httr2::resp_body_string(resp)

  # Don't pass empty strings into the TSV parser.
  if (!nzchar(trimws(bls_str_tsv))) {
    rlang::abort(
      message = "Response body is empty; cannot parse TSV.",
      class   = "get_bls_data_empty_body",
      bls_url = httr2::resp_url(resp),
      headers = httr2::resp_headers(resp)
    )
  }

  ### Parse TSV ###
  # Use readr to parse TSV text from an in-memory string connection.
  # - col_types: read all columns as character to avoid guesswork and
  #   let downstream code do explicit type conversions.
  # - trim_ws: trim whitespace in fields.
  bls_df <- readr::read_tsv(
    file      = I(bls_str_tsv),
    col_names = TRUE,
    col_types = readr::cols(.default = readr::col_character()),
    progress  = readr::show_progress(),
    trim_ws   = TRUE
  )

  # Return the tibble to the caller.
  return(bls_df)
}

