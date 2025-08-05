test_that("computes index correctly for default parameters", {
  expect_equal(create_index(c(100, 120, 130)), c(100, 120, 130))
})

test_that("computes index relative to 0-centered scale", {
  result <- create_index(c(100, 120, 130), idx_type = 0)
  expect_equal(result, c(0, 20, 30))
})

test_that("index works with custom idx_pos", {
  result <- create_index(c(80, 100, 120), idx_pos = 2)
  expect_equal(round(result, 2), round(c(80/100, 100/100, 120/100) * 100, 2))
})

test_that("throws error for base value = 0", {
  expect_error(
    create_index(c(0, 100, 200), idx_pos = 1),
    "base_value.*invalid"
  )
})

test_that("throws error for base value = NA", {
  expect_error(
    create_index(c(100, NA, 300), idx_pos = 2),
    "base_value.*invalid"
  )
})

test_that("throws error for base value = Inf", {
  expect_error(
    create_index(c(100, Inf, 300), idx_pos = 2),
    "base_value.*invalid"
  )
})

test_that("warns if non-base values contain NA, NaN, or Inf", {
  expect_warning(
    create_index(c(100, 200, NA), idx_pos = 1),
    "Some values.*NA, NaN, or Inf"
  )
  expect_warning(
    create_index(c(100, 200, Inf), idx_pos = 1),
    "Some values.*NA, NaN, or Inf"
  )
})

test_that("throws error if num_vec length < 2", {
  expect_error(
    create_index(c(100)),
    "length greater than 1"
  )
})

test_that("throws error if idx_pos is out of bounds", {
  expect_error(
    create_index(c(100, 200), idx_pos = 3),
    "idx_pos.*within the bounds"
  )
})

test_that("throws error for invalid idx_type", {
  expect_error(
    create_index(c(100, 120), idx_type = 50),
    "idx_type.*either 100 or 0"
  )
})
