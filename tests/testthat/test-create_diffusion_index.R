test_that("Federal Reserve calculation works as expected", {
  result <- create_diffusion_index(pct_increased = c(0.6, 0.5), pct_decreased = c(0.2, 0.3))
  expect_equal(result, c(40, 20))
})

test_that("IHS-PMI calculation works as expected", {
  result <- create_diffusion_index(pct_increased = 0.6, pct_unchanged = 0.3, idx_type = "IHS-PMI")
  expect_equal(result, 0.6 + 0.5 * 0.3)
})

test_that("Conference Board calculation works as expected", {
  result <- create_diffusion_index(pct_change = c(0.001, -0.001, 0), idx_type = "Conference Board")
  expect_equal(result, mean(c(1, 0, 0.5)))
})

test_that("mismatched lengths throw errors", {
  expect_error(
    create_diffusion_index(pct_increased = c(0.6, 0.5), pct_decreased = 0.2),
    "must be the same length"
  )

  expect_error(
    create_diffusion_index(pct_increased = 0.6, pct_unchanged = c(0.3, 0.4), idx_type = "IHS-PMI"),
    "must be the same length"
  )
})

test_that("missing required arguments throw errors", {
  expect_error(
    create_diffusion_index(pct_decreased = 0.2),
    "must be provided for 'Federal Reserve'"
  )

  expect_error(
    create_diffusion_index(pct_increased = 0.6, idx_type = "IHS-PMI"),
    "must be provided for 'IHS-PMI'"
  )

  expect_error(
    create_diffusion_index(idx_type = "Conference Board"),
    "must be provided for 'Conference Board'"
  )
})

test_that("extra arguments trigger warnings", {
  expect_warning(
    create_diffusion_index(pct_increased = 0.6, pct_decreased = 0.2, pct_unchanged = 0.1),
    "Ignoring 'pct_unchanged'"
  )

  expect_warning(
    create_diffusion_index(pct_increased = 0.6, pct_decreased = 0.2, pct_change = 0.1),
    "Ignoring 'pct_change'"
  )

  expect_warning(
    create_diffusion_index(pct_increased = 0.6, pct_unchanged = 0.3, pct_decreased = 0.2, idx_type = "IHS-PMI"),
    "Ignoring 'pct_decreased'"
  )
})

test_that("NAs in pct_increased or pct_decreased warn but return NA", {
  expect_warning(
    result <- create_diffusion_index(pct_increased = c(0.6, NA), pct_decreased = c(0.2, 0.3)),
    "contains NA"
  )
  expect_true(is.na(result[2]))
})

test_that("NAs in pct_change for Conference Board throw an error", {
  expect_error(
    create_diffusion_index(pct_change = c(0.001, NA), idx_type = "Conference Board"),
    "contains NA"
  )
})

test_that("non-numeric inputs are rejected", {
  expect_error(
    create_diffusion_index(pct_increased = "0.6", pct_decreased = 0.2),
    "must be numeric"
  )

  expect_error(
    create_diffusion_index(pct_change = Inf, idx_type = "Conference Board"),
    "must contain only finite values"
  )
})
