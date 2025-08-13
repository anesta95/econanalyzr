# tests/testthat/test-create_diffusion_index.R

test_that("Federal Reserve calculation works as expected", {
  result <- create_diffusion_index(
    pct_increased = c(0.6, 0.5),
    pct_decreased = c(0.2, 0.3)
  )
  expect_equal(result, c(40, 20))
})

test_that("IHS-PMI calculation works as expected", {
  result <- create_diffusion_index(
    pct_increased = 0.6,
    pct_unchanged  = 0.3,
    idx_type = "IHS-PMI"
  )
  expect_equal(result, (0.6 + 0.5 * 0.3) * 100)
})

test_that("Conference Board calculation works as expected", {
  result <- create_diffusion_index(
    pct_change = c(0.001, -0.001, 0),
    idx_type = "Conference Board"
  )
  expect_equal(result, mean(c(1, 0, 0.5)) * 100)
})

test_that("mismatched lengths throw errors", {
  expect_error(
    create_diffusion_index(
      pct_increased = c(0.6, 0.5),
      pct_decreased = 0.2
    ),
    regexp = "same length"
  )
  expect_error(
    create_diffusion_index(
      pct_increased = 0.6,
      pct_unchanged  = c(0.3, 0.4),
      idx_type = "IHS-PMI"
    ),
    regexp = "same length"
  )
})

test_that("missing required arguments throw errors", {
  expect_error(
    create_diffusion_index(pct_decreased = 0.2),
    regexp = "must be provided for 'Federal Reserve'"
  )
  expect_error(
    create_diffusion_index(pct_increased = 0.6, idx_type = "IHS-PMI"),
    regexp = "must be provided for 'IHS-PMI'"
  )
  expect_error(
    create_diffusion_index(idx_type = "Conference Board"),
    regexp = "must be provided for 'Conference Board'"
  )
})

test_that("extra arguments trigger warnings", {
  # Federal Reserve: extra pct_unchanged only
  expect_warning(
    create_diffusion_index(
      pct_increased = 0.6,
      pct_decreased = 0.2,
      pct_unchanged = 0.1
    ),
    regexp = "Ignoring 'pct_unchanged'"
  )

  # Federal Reserve: extra pct_change
  expect_warning(
    create_diffusion_index(
      pct_increased = 0.6,
      pct_decreased = 0.2,
      pct_change = 0.1
    ),
    regexp = "Ignoring 'pct_unchanged' and 'pct_change'|Ignoring 'pct_change'"
  )

  # IHS-PMI: extra pct_decreased
  expect_warning(
    create_diffusion_index(
      pct_increased = 0.6,
      pct_unchanged = 0.3,
      pct_decreased = 0.2,
      idx_type = "IHS-PMI"
    ),
    regexp = "Ignoring 'pct_decreased'"
  )
})

test_that("NAs in pct_increased or pct_decreased warn and propagate NA (Federal Reserve)", {
  expect_warning(
    res <- create_diffusion_index(
      pct_increased = c(0.6, NA),
      pct_decreased = c(0.2, 0.3)
    ),
    regexp = "pct_increased.*contains NA|contains NA"
  )
  expect_true(is.na(res[2]))

  expect_warning(
    res2 <- create_diffusion_index(
      pct_increased = NA_real_,
      pct_decreased = 0.2
    ),
    regexp = "pct_increased.*contains NA|contains NA"
  )
  expect_true(is.na(res2))
})

test_that("NAs in pct_unchanged warn and propagate NA (IHS-PMI)", {
  expect_warning(
    res <- create_diffusion_index(
      pct_increased = c(0.6, 0.5),
      pct_unchanged = c(0.3, NA),
      idx_type = "IHS-PMI"
    ),
    regexp = "pct_unchanged.*contains NA|contains NA"
  )
  expect_true(is.na(res[2]))

  expect_warning(
    res2 <- create_diffusion_index(
      pct_increased = NA_real_,
      pct_unchanged = 0.3,
      idx_type = "IHS-PMI"
    ),
    regexp = "pct_increased.*contains NA|contains NA"
  )
  expect_true(is.na(res2))
})

test_that("NAs in pct_change (Conference Board) throw an error", {
  expect_error(
    create_diffusion_index(
      pct_change = c(0.001, NA),
      idx_type = "Conference Board"
    ),
    regexp = "pct_change.*must not contain NA"
  )
  expect_error(
    create_diffusion_index(
      pct_change = NA_real_,
      idx_type = "Conference Board"
    ),
    regexp = "pct_change.*must not contain NA"
  )
})

test_that("non-numeric inputs are rejected and non-finite values are rejected", {
  expect_error(
    create_diffusion_index(
      pct_increased = "0.6",
      pct_decreased = 0.2
    ),
    regexp = "must be numeric"
  )
  expect_error(
    create_diffusion_index(
      pct_increased = c(0.6, Inf),
      pct_decreased = c(0.2, 0.3)
    ),
    regexp = "only finite values"
  )
  expect_error(
    create_diffusion_index(
      pct_change = NaN,
      idx_type = "Conference Board"
    ),
    regexp = "only finite values"
  )
})

# ---- NEW: bounds tests for proportion-type inputs ----

test_that("proportion inputs must be within [0, 1]", {
  # Federal Reserve
  expect_error(
    create_diffusion_index(
      pct_increased = c(1.1, 0.5),
      pct_decreased = c(0.2, 0.3)
    ),
    regexp = "between 0 and 1"
  )
  expect_error(
    create_diffusion_index(
      pct_increased = c(-0.1, 0.5),
      pct_decreased = c(0.2, 0.3)
    ),
    regexp = "between 0 and 1"
  )
  expect_error(
    create_diffusion_index(
      pct_increased = 0.6,
      pct_decreased = 1.5
    ),
    regexp = "between 0 and 1"
  )

  # IHS-PMI
  expect_error(
    create_diffusion_index(
      pct_increased = 0.6,
      pct_unchanged = 1.2,
      idx_type = "IHS-PMI"
    ),
    regexp = "between 0 and 1"
  )
  expect_error(
    create_diffusion_index(
      pct_increased = -0.2,
      pct_unchanged = 0.3,
      idx_type = "IHS-PMI"
    ),
    regexp = "between 0 and 1"
  )
})

test_that("pct_change may be any finite real (outside [0,1] allowed)", {
  res <- create_diffusion_index(
    pct_change = c(1.2, -0.8, 0),
    idx_type = "Conference Board"
  )
  # Encoded: 1.2 -> 1; -0.8 -> 0; 0 -> 0.5; mean = 0.5 * 100
  expect_equal(res, 0.5 * 100)
})
