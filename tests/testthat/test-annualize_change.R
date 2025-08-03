test_that("annualize_change computes correct rates with 'up' direction", {
  result <- annualize_change(
    start_values = c(100, 200, 300),
    end_values = c(110, 220, 330),
    time_elapsed = c(1, 2, 3),
    time_unit = "monthly",
    projection_direction = "up"
  )

  expected <- (c(110, 220, 330) / c(100, 200, 300))^(12 / c(1, 2, 3)) - 1
  expect_equal(result, expected)
})

test_that("annualize_change computes correct rates with 'down' direction", {
  result <- annualize_change(
    start_values = c(100, 150),
    end_values = c(160, 300),
    time_elapsed = c(8, 16),  # quarters
    time_unit = "quarterly",
    projection_direction = "down"
  )

  expected <- (c(160, 300) / c(100, 150))^(1 / (c(8, 16) / 4)) - 1
  expect_equal(result, expected)
})

test_that("scalar time_elapsed is auto-recycled", {
  result <- annualize_change(
    start_values = c(100, 150),
    end_values = c(110, 165),
    time_elapsed = 1,
    time_unit = "monthly",
    projection_direction = "up"
  )

  expected <- (c(110, 165) / c(100, 150))^(12) - 1
  expect_equal(result, expected)
})

test_that("non-numeric inputs raise errors", {
  expect_error(annualize_change("100", 110, 1, "monthly", "up"))
  expect_error(annualize_change(100, "110", 1, "monthly", "up"))
  expect_error(annualize_change(100, 110, "1", "monthly", "up"))
})

test_that("negative or zero values raise errors", {
  expect_error(annualize_change(c(100, -100), c(110, 120), 1, "monthly", "up"))
  expect_error(annualize_change(c(100, 100), c(0, 120), 1, "monthly", "up"))
  expect_error(annualize_change(c(100, 100), c(110, 120), 0, "monthly", "up"))
})

test_that("mismatched vector lengths raise errors", {
  expect_error(annualize_change(c(100, 150), c(110), 1, "monthly", "up"))
  expect_error(annualize_change(c(100, 150), c(110, 160), c(1, 2, 3), "monthly", "up"))
})

test_that("invalid time_unit or projection_direction raises errors", {
  expect_error(annualize_change(100, 110, 1, time_unit = "yearly", projection_direction = "up"))
  expect_error(annualize_change(100, 110, 1, time_unit = "monthly", projection_direction = "sideways"))
})

