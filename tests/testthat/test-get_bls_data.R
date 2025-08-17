# tests/testthat/test-get_bls_data.R

testthat::test_that("get_bls_data: local web server success & failures", {
  testthat::skip_on_cran()

  app <- webfakes::new_app()

  # 200 OK, text/plain, valid TSV
  app$get("/ok", function(req, res) {
    res$set_status(200L)
    res$set_type("text/plain")
    res$send("series_id\tyear\tperiod\tvalue\nJTALL\t2020\tM01\t1.23\n")
  })

  # 200 OK, text/tab-separated-values, valid TSV
  app$get("/ok-ttsv", function(req, res) {
    res$set_status(200L)
    res$set_type("text/tab-separated-values")
    res$send("series_id\tyear\tperiod\tvalue\nJTALL\t2020\tM02\t2.34\n")
  })

  # 200 OK but wrong content-type
  app$get("/wrong-ctype", function(req, res) {
    res$set_status(200L)
    res$set_type("application/json")
    res$send('series_id\tyear\tperiod\tvalue\nJTALL\t2020\tM01\t1.23\n')
  })

  # 200 OK but empty body
  app$get("/empty", function(req, res) {
    res$set_status(200L)
    res$set_type("text/plain")
    res$send("")  # nothing
  })

  # 204 No Content
  app$get("/no-body", function(req, res) {
    res$set_status(204L)
    res$send("")
  })

  # 500 with a short diagnostic body
  app$get("/error500", function(req, res) {
    res$set_status(500L)
    res$set_type("text/plain")
    res$send("Internal server error details")
  })

  srv <- webfakes::new_app_process(app)
  on.exit(srv$stop(), add = TRUE)
  base <- srv$url()

  # ---- Success cases --------------------------------------------------------
  df1 <- get_bls_data(paste0(base, "/ok"), "me@example.com")
  testthat::expect_s3_class(df1, "tbl_df")
  testthat::expect_named(df1, c("series_id", "year", "period", "value"))
  testthat::expect_true(all(vapply(df1, inherits, logical(1), what = "character")))
  testthat::expect_equal(nrow(df1), 1L)

  df2 <- get_bls_data(paste0(base, "/ok-ttsv"), "me@example.com")
  testthat::expect_s3_class(df2, "tbl_df")
  testthat::expect_identical(df2$period, "M02")

  # ---- Input validation -----------------------------------------------------
  testthat::expect_error(get_bls_data(123, "me@example.com"), class = "get_bls_data_bad_scalar")
  testthat::expect_error(get_bls_data(paste0(base, "/ok"), 123), class = "get_bls_data_bad_scalar")
  testthat::expect_error(get_bls_data("ftp://example.com/file.tsv", "me@example.com"), class = "get_bls_data_bad_url")
  testthat::expect_error(get_bls_data(paste0(base, "/ok"), "not-an-email"), class = "get_bls_data_bad_email")

  # ---- HTTP error path (non-2xx) -------------------------------------------
  cnd500 <- testthat::expect_error(
    get_bls_data(paste0(base, "/error500"), "me@example.com"),
    class = "get_bls_data_http_error"
  )
  testthat::expect_true(inherits(cnd500, "get_bls_data_http_error"))
  testthat::expect_identical(cnd500$status, 500L)
  testthat::expect_true(is.character(cnd500$body_snippet))
  # header names are lowercased by httr2
  testthat::expect_true(!is.null(cnd500$headers[["content-type"]]))

  # ---- No body present ------------------------------------------------------
  testthat::expect_error(
    get_bls_data(paste0(base, "/no-body"), "me@example.com"),
    class = "get_bls_data_no_body"
  )

  # ---- Wrong content type ---------------------------------------------------
  cnd_ctype <- testthat::expect_error(
    get_bls_data(paste0(base, "/wrong-ctype"), "me@example.com"),
    class = "get_bls_data_bad_content_type"
  )
  testthat::expect_true(inherits(cnd_ctype, "get_bls_data_bad_content_type"))
  testthat::expect_true(!is.null(cnd_ctype$headers[["content-type"]]))

  # ---- Empty body (200 + empty) -> treated as no body by httr2 -------------
  testthat::expect_error(
    get_bls_data(paste0(base, "/empty"), "me@example.com"),
    class = "get_bls_data_no_body"
  )
})


